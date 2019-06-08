### FUNCTION: data_binormal
#' @export
data_binormal <- function(nV = 200,
                          nE=200,
                          prev = 0.2,
                          auc=0.9,
                          gridsize=1000,
                          data =NULL,
                          job = NULL){
  args = as.list(environment())
  mu0 <- 0; sd0 <- 1; sd1 <- 1;
  mu1 <- sqrt(2)*qnorm(auc)

  nV1 <- rbinom(1, nV, prev); nV0 <- nV - nV1
  nE1 <- rbinom(1, nE, prev); nE0 <- nE - nE1

  V <- sample_binormal(nV1, nV0, mu1, sd1, mu0, sd0)
  E <- sample_binormal(nE1, nE0, mu1, sd1, mu0, sd0)

  # derive info object
  info <- data.frame(dataset = c("pop", "eval", "train", "val", "learn"),
                     seed = rep(NA, 5),
                     n = c(NA, nE, 0, nV, nV),
                     n1 = c(NA, sum(E$D), 0, sum(V$D), sum(V$D)))
  rownames(info) <- info$dataset
  info$prev <- info$n1 / info$n; info["pop", "prev"] <- prev

  # cutpoints: data based / grid based
  t.data <- sort(unique(V$Y))
  t.grid <- seq(min(V$Y), max(V$Y), length.out = gridsize)
  t.comb <- c(t.data, t.grid)
  mn <- c(paste0("data", 1:length(t.data)), paste0("grid", 1:length(t.grid)))
  hp <- list(data = data.frame(threshold=t.data),
             grid = data.frame(threshold=t.grid))
  rownames(hp$data) <- mn[1:length(t.data)]
  rownames(hp$grid) <- mn[-(1:length(t.data))]

  # true performances (no re-training):
  theta <- true_performance(t.comb, prev, mu1, sd1, mu0, sd0, models=mn)[,c("acc", "se", "sp")]
  names(theta) <- c("theta", "theta1", "theta0")

  # create model pseudo objects
  train.models <- list(train = NA,
                       val = list(pred = t2pred(t.comb, V, mn), labels = V$D),
                       theta = theta)
  train.models$val$thetahat <- predict_theta(train.models$val$pred, train.models$val$labels, T)

  learn.models <- list(learn = NA,
                       eval = list(pred = t2pred(t.comb, E, mn), labels = E$D),
                       theta = theta)
  train.models$val$thetahat <- predict_theta(learn.models$eval$pred, learn.models$eval$labels, T)

  return(list(job = list(id=job$id, seed=job$seed, pars=job$pars),
              info = info,
              models = mn,
              hyperparams = hp,
              train.models = train.models,
              learn.models = learn.models))
}

######
# require(dplyr)
# instance <- SEPM.MLE:::load_data(folder = "D:/SIM/MLE_SIM/DATA/DATA_CHECK", base = "job", id=1)
# instance$train.models$theta %>% str(1)
# instance$info
#
# instance <- data_binormal()
# SEPM.MLE::study_accuracy(methods="data", instance=data_binormal())
#SEPM.MLE::study_accuracy(methods="data", select.method="se", select.args="c=1", instance=instance)$result
# SEPM.MLE:::get_hp("data163")
# ?study_accuracy


##########
t2pred <- function(t, D, models=NULL){
  pred <- Matrix::Matrix(sapply(t, function(u) as.numeric(D$Y > u)), sparse=T)
  colnames(pred) <- models
  return(pred)
}

sample_binormal <- function(n1, n0, mu1, sd1, mu0, sd0){
  data.frame(D = c(rep(1, n1), rep(0, n0)),
             Y = c(rnorm(n1, mu1, sd1), rnorm(n0, mu0, sd0)))
}

roc_binormal <- function(mu1, sd1=1, mu0=0, sd0=1){
  a <- (mu1-mu0)/sd1
  b <- sd0/sd1
  function(t){pnorm(a+b*qnorm(t))}
}

auc_binormal <- function(mu1, sd1=1, mu0=0, sd0=1){
  # Pepe, p. 83
  a <- (mu1-mu0)/sd1
  b <- sd0/sd1
  pnorm(a/sqrt(1+b^2))
}

opt_threshold <- function(prev, mu1, sd1=1, mu0=0, sd0=1, method="sesp"){
  t <- seq(0,1,1e-5); tt = mu0-sd0*qnorm(t)
  R <- true_performance(t=tt, prev=prev, mu1, sd1=sd1, mu0=mu0, sd0=sd0, models=NULL)
  mopt <- switch(method,
                 sesp = which.max(R$sesp),
                 bacc = which.max(R$bacc),
                 acc = which.max(R$acc))
  return(R[mopt, ])
}

true_performance <- function(t, prev, mu1, sd1=1, mu0=0, sd0=1, models=NULL){
  R <- data.frame(t = t, se=pnorm((mu1-t)/sd1), sp=1-pnorm((mu0-t)/sd0))
  R <- dplyr::mutate(R, sesp=pmin(se, sp), bacc=0.5*se+0.5*sp, acc=prev*se+(1-prev)*sp)
  rownames(R) <- models
  return(R)
}





