#' Generate artifical data and train prediction models
#'
#' This function is used to generate artifical observations of feature-label pairs which are then
#' used to train prediction models by various machine learning algorithms.
#'
#' @param n.learn integer, number of learning observations
#' @param ratio.lv numeric, fraction of learning observations to use for validation (default: 0.75)
#' @param n.eval integer, number of test observations
#' @param n.pop integer, population sample size (used to calculate 'true' performances)
#' @param P integer, total number of features (needs to be larger than active features of scenario)
#' @param red integer, set degree of redundancy among features (independent features with red=0)
#' @param rho numeric, (equi)correlation in (0,1) between redundant features (no effect if red=0)
#' @param scenario character, specifies data distribution scenario, e.g. "EOMPM_A"
#' @param scenario.learn character, potentially perturbed learning distribution, equalls scenario by default
#' @param methods character, specify learning algorithms, e.g. "glmnet|xgbTree" (use exact method names as in caret!)
#' @param M integer, number of models to train per learning algorithm
#' @param tuning character, specify hyperparameter sampling, either "grid" or "random"
#' @param data NULL, batchtools argument
#' @param job  NULL, batchtools argument
#'
#' @return Returns a data instance which is processed by either the study_accuracy() or theory().
#'
#' @export
generate_data <- function(n.learn = 400,
                          ratio.lv = 0.75,
                          n.eval = 10000,
                          n.pop = 100000,
                          P = 50,
                          red = 0,
                          rho = 0.5,
                          scenario = "EOMPM_A",
                          scenario.learn = scenario,
                          methods = "glmnet",
                          M = 10,
                          tuning = "random",
                          data = NULL,
                          job = NULL){
  ## Calculate sample sizes:
  n.train <- round(ratio.lv * n.learn)
  n.val <- n.learn - n.train
  #n.eval <- round(ratio.le * n.learn)

  ## Generate all data sets in feature/label space:
  seeds <- c(1, round(runif(3, 100, 100000)))

  stopifnot(all(c(scenario, scenario.learn) %in% names(SCENARIOS)))
  S <- SCENARIOS[[scenario]]; S.L <- SCENARIOS[[scenario.learn]]
  if(!is.null(P)){S$pars$P <- S.L$pars$P <- P}
  if(!is.null(red)){S$pars$red <- S.L$pars$red <- red}
  if(!is.null(rho)){S$pars$rho <- S.L$pars$rho <- rho}

  samples <- list()
  samples[["pop"]]   <- draw_sample(n=n.pop,   score=S$score,   pars=S$pars,   seed=1)
  samples[["eval"]]  <- draw_sample(n=n.eval,  score=S$score,   pars=S$pars,   seed=seeds[2])
  samples[["train"]] <- draw_sample(n=n.train, score=S.L$score, pars=S.L$pars, seed=seeds[3])
  samples[["val"]]   <- draw_sample(n=n.val,   score=S.L$score, pars=S.L$pars, seed=seeds[4])
  samples[["learn"]] <- combine_samples(samples$train, samples$val)

  #names(samples) <- c("pop", "train", "val", "eval")
  #samples$learn <- combine_samples(samples$train, samples$val)

  ## Train all models:
  methods <- strsplit(methods, "_")[[1]]
  models  <- training(method=methods, M=rep(M, length(methods)), tuning=tuning, samples=samples)

  ## Calculate predictions and true performances:
  train.models <- learn.models <- list()

  train.models$train <- list(pred   = get_predictions(models$train.models, newdata=samples$train$X),
                             labels = samples$train$Y)
  train.models$val   <- list(pred   = get_predictions(models$train.models, newdata=samples$val$X),
                             labels = samples$val$Y)

  learn.models$learn <- list(pred   = get_predictions(models$learn.models, newdata=samples$learn$X),
                             labels = samples$learn$Y)
  learn.models$eval  <- list(pred   = get_predictions(models$learn.models, newdata=samples$eval$X),
                             labels = samples$eval$Y)

  ## Derive estimated and true performances (acc, sens, spec)
  ## train models:
  train.models <- lapply(train.models,
                         function(x){x$thetahat = predict_theta(x$pred, x$labels, stratify = T); x})
  train.models$theta <- predict_theta(pred = get_predictions(models$train.models,
                                                             newdata=samples$pop$X),
                                      labels = samples$pop$Y, stratify = TRUE)
  ## learn models:
  learn.models <- lapply(learn.models,
                         function(x){x$thetahat = predict_theta(x$pred, x$labels, stratify = T); x})
  learn.models$theta <- predict_theta(pred = get_predictions(models$learn.models,
                                                             newdata=samples$pop$X),
                                      labels = samples$pop$Y, stratify = TRUE)

  ## Derive some data info:
  info <- data.frame(dataset = names(samples),
                     seed = c(seeds, NA),
                     n = sapply(samples, function(s)length(s$Y)),
                     n1 = sapply(samples, function(s)sum(s$Y)))
  info$prev <- info$n1 / info$n

  return(list(job = list(id=job$id, seed=job$seed, pars=job$pars),
              info = info,
              models = names(models$learn.models),
              hyperparams = models$hp,
              train.models = train.models,
              learn.models = learn.models))
}



### FUNCTION: get_prediction
get_prediction <- function(model, newdata){
  #fac2num(caret::predict.train(model, newdata = newdata, type="raw"))
  fac2num(predict(model, newdata = newdata, type="raw"))
}

### FUNCTION: get_predictions
#' @importFrom Matrix Matrix
get_predictions <- function(models, newdata){
  Matrix::Matrix(sapply(models, get_prediction, newdata=newdata), sparse=T)
}

### FUNCTON: predict_theta
predict_theta <- function(pred, labels, stratify=FALSE){
  stopifnot(max(nrow(labels), length(labels)) == nrow(pred))

  C <- as.matrix(pred == labels)
  result <- data.frame(theta = colMeans(C))

  u <- unique(labels)
  if(stratify & length(u) <= 2){
    for(l in u){
      result[,paste0("theta", l)] <- colMeans(C[labels==l, ])
    }
  }

  return(result)
}

### FUNCTION: combine_samples
combine_samples <- function(d1, d2){
  list(Y=c(d1$Y, d2$Y), X=rbind(d1$X, d2$X))
}


# ### FUNCTION: config2args
# config2args <- function(x){ # assumes character of format "method1:M1_method2:M2_ ... _methodK:MK"
#   L <- lapply(unlist(strsplit(x, "_")), function(y) unlist(strsplit(y, ":")))
#   list(method=sapply(L, function(x)x[1]),
#        M = as.numeric(sapply(L, function(x)x[2])))
# }


