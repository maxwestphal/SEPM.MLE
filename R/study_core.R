### TEST:
#   instance <- load_data(folder=paste0("E:/DataScience/R/SIM/MLE_SIM", "/DATA"))
#   methods = "glmnet"; M =50; M.start=1; M.probs = NA; M.seed=1
#   select.method="simplest.en"; select.args=NA; select.limit=Inf; eval.n=400
#   str(sample_setM(instance, methods=NA, M=100, M.start=26), 2)

# FUNCTION: sample_setM
sample_setM <- function(instance,
                        methods=NA, # NA or specify methods to be matched, e.g. "glmnet|xgbTree"
                        M=200,
                        M.start = NA, # if = NA, sample randomly according to M.probs
                        M.probs = "uniform",
                        M.seed = 1){
  # return all models if required:
  if(M>=length(instance$models)){
    return(instance)
  }
  # derive number of model types
  if(is.na(methods)){
    methods <- "glmnet|xgbTree|rpartCost|svmLinearWeights2"
  }
  methods.num <- length(strsplit(methods, "\\|")[[1]])
  # setM0 = setM after reduction to relevant methods:
  setM0 <- instance$models[grepl(methods, instance$models)]
  M0 <- length(setM0); stopifnot(M <= M0)
  # if no start index is given: sample randomly according probs defined by M.probs
  if(is.na(M.start)){
    set.seed(M.seed)
    pr <- switch(M.probs, uniform = rep(1,M0),
                 learn.theta = instance$learn.models$theta[setM0, "theta"],
                 learn.theta.squared = instance$learn.models$theta[setM0, "theta"]^2,
                 learn.theta.neg = 1-instance$learn.models$theta[setM0, "theta"],
                 learn.theta.neg.squared = (1-instance$learn.models$theta[setM0, "theta"])^2)
    subs <- sample(M0, M, replace=FALSE, prob=pr)
  }else{
    b <- M/methods.num; stopifnot(b %% 1 == 0)
    subs <- as.integer(sapply((0:(methods.num-1))*(M0/methods.num),function(x){x+M.start:(M.start+b-1)}))
  }
  sel <- instance$models %in% setM0[subs]
  # rebuilt instance based on selected models
  out <- instance[1:2]
  out$models <- instance$models[sel]
  out$hyperparams <- lapply(instance$hyperparams, function(h){h[rownames(h) %in% out$models, ,drop=FALSE]})
  out$train.models <- subset_models(instance$train.models, sel)
  out$learn.models <- subset_models(instance$learn.models, sel)
  return(out)
}

### FUNCTION: subset_models
subset_models <- function(models, sel){
  out <- lapply(models[1:2], function(x){
    if(is.list(x)){
      x$pred <- x$pred[,sel]; x$thetahat <- x$thetahat[sel, ]; return(x)
    }})
  out$theta <- models$theta[sel, ]
  return(out)
}

### FUNCTION: string2list
string2list <- function(s, sep1="_", sep2="=", convert=function(x)x){
  l <- strsplit(s, split=sep1)[[1]]
  ll <- strsplit(l, split=sep2)
  a <- lapply(ll, function(x)x[2])
  names(a) <- lapply(ll, function(x)x[1])
  a <- lapply(a, convert)
  return(a)
}

### FUNCTION: unique_max
unique_max <- function(x){
  out <- which(x == max(x))
  if(length(out)==1){
    return(out)
  }else{
    return(sample(out, 1))
  }
}

### FUNCTION: ave_max
ave_max <- function(x, aggr=mean){
  aggr(x[which(x == max(x))])
}

### FUNCTION: all_max
all_max <- function(x, aggr=mean){
  which(x == max(x))
}

### FUNCTION: preproc_select
preproc_select <- function(select.method, select.args, select.limit, instance, eval.n){
  average <- FALSE
  delta <- instance$learn.models$theta$theta - instance$train.models$theta$theta
  select.max <- Inf
  if(select.limit == "sqrt"){
    select.max <- round(sqrt(eval.n))
  }
  if(!select.method %in% c("oracle", "ensemble")){
    select.args <- string2list(select.args, convert=as.numeric)
  }
  if(select.method == "oracle"){
    select.method <- "user"
    if(select.args=="delta"){
      tt <- instance$train.models$val$thetahat$theta + delta
    }else{
      tt <- instance[[paste0(select.args, ".models")]]$theta$theta
    }
    select.args <- list(s=which(tt==max(tt)))
  }
  if(select.method == "simplest.en"){
    select.method <- "user"
    hp.en <- instance$hyperparams$glmnet
    stopifnot(nrow(hp.en)>0)
    mm <- instance$models %in% rownames(hp.en)
    tt <- instance$train.models$val$thetahat$theta[mm]; theta.max <- max(tt)
    incl <- tt > theta.max - 1*sqrt(theta.max*(1-theta.max)/instance$info["val", "n"])
    pp <- hp.en$alpha * hp.en$lambda; pp[!incl] <- -Inf
    select.args <-list(s=which(instance$models %in% rownames(hp.en)[pp==max(pp)]))
  }
  if(select.method == "ensemble"){
    select.method <- "se"
    average <- TRUE
  }
  if(select.method == "weighted.acc"){
    w0 = w1 = 0.5
    if(!is.null(select.args$w0)){
      w0 <- select.args$w0
    }
    if(!is.null(select.args$w1)){
      w1 <- select.args$w1
    }
    select.method = "user"
    n <- instance$info["val", "n"]
    n1 <- instance$info["val", "n1"]
    n0 <- n - n1
    se.val <- instance$train.models$val$thetahat$theta1
    sp.val <- instance$train.models$val$thetahat$theta0
    ba <- w1*se.val + w0*sp.val
    opt <- sample(rep(which(ba == max(ba)), 2), 1) ## TODO: unique_max ????
    se.ba <- (sqrt( (w1^2)*se.val*(1-se.val)/n1 + (w0^2)*sp.val*(1-sp.val)/n0))[opt]
    sel <- which(abs(ba - ba[opt]) <= select.args$c*se.ba & rank(-ba, ties.method="random") <= select.max)
    select.args <- list(s= sel)
  }
  if(select.method == "max.min"){
    select.method = "user"
    n <- instance$info["val", "n"]
    n1 <- instance$info["val", "n1"]
    n0 <- n - n1
    se0 <- sp0 <- 0.5; cc <- 1
    if(!is.null(select.args$se0)){
      se0 <- select.args$se0
    }
    if(!is.null(select.args$sp0)){
      w1 <- select.args$w1
    }
    if(!is.null(select.args$c)){
      cc <- select.args$c
    }
    se.val <- instance$train.models$val$thetahat$theta1
    sp.val <- instance$train.models$val$thetahat$theta0
    se.se <-  sqrt(se.val*(1-se.val)/n1); sp.se <- sqrt(sp.val*(1-sp.val)/n0)

    D <- data.frame(tse =  (se.val-se0)/se.se, tsp= (sp.val-sp0)/sp.se) %>%
      dplyr::mutate(tmin = pmin(tse, tsp),
                    r = rank(-tmin, ties.method = "random"),
                    s = abs(tmin-max(tmin)) <= cc & r <= select.max)
    select.args <- list(s= which(D$s))
  }
  select.args$select.max = select.max
  return(list(sm=select.method, sa=select.args, av=average, delta=delta))
}

### FUNCTION: get_obs
get_obs <- function(rdm.eval, first.eval, n.eval, n.max=NULL){
  #stopifnot(n.eval <= 10000)
  if(n.eval == Inf){
    return(1:n.max)
  }
  if(rdm.eval){
    obs.eval <- sample(1:length(learn.models$eval$labels), n.eval, replace=FALSE)
  }else{
    obs.eval <- first.eval:(first.eval+n.eval-1)
  }
  return(obs.eval)
}

### FUNCTION: derive_result
derive_result <- function(load.id, instance, pred, inf){
  result <- data.frame(load.id=load.id, opt.theta = max(instance$learn.models$theta$theta),
                       S=ncol(pred), as.data.frame(inf$inference$info[3:5]))

  mstar <- unique_max(inf$inference$result$teststat)
  final.mod.name <- as.character(inf$inference$result[mstar, "model.name"])
  final.mod.index <- which(final.mod.name== instance$models)
  final.mod.result <- derive_stats(final.mod.index, "final.mod", instance)
  final.mod.result$final.mod.thetabar <- inf$estimation$result$theta.bar[final.mod.name]

  final.mod.result <- cbind(final.mod.result,
                            inf$inference$result[mstar, c("estimate", "corrected", "lower", "upper")])
  colnames(final.mod.result)[(ncol(final.mod.result)-3):ncol(final.mod.result)] <-
    paste0("final.mod.", c("thetahat", "thetatilde", "lower", "upper"))

  return(cbind(result, final.mod.result))
}

### FUNCTION: derive_result_sesp
derive_result_sesp <- function(load.id, instance, pred, inf, delta=0){
  result <- data.frame(load.id=load.id,
                       opt.acc = max(instance$learn.models$theta$theta),
                       opt.se = max(instance$learn.models$theta$theta1),
                       opt.sp = max(instance$learn.models$theta$theta0),
                       S=ncol(pred), as.data.frame(inf$inference$info[3:5]))

  mopt <- unique_max(pmin(instance$learn.models$theta$theta0,
                          instance$learn.models$theta$theta1 -delta))
  result$opt.mod.name <- as.character(instance$models[mopt])
  result$opt.sp.cpe <- instance$learn.models$theta$theta0[mopt]
  result$opt.se.cpe <- instance$learn.models$theta$theta1[mopt]

  mstar <- unique_max(pmin(inf$inference$result.0$lower,
                           inf$inference$result.1$lower - delta))
  final.mod.name <- as.character(inf$inference$result.1[mstar, "model.name"])
  final.mod.index <- which(final.mod.name== instance$models)
  final.mod.result <- derive_stats(final.mod.index, "final", instance, details=TRUE)
  final.mod.result$final.se.bar <- inf$estimation$result.1$theta.bar[final.mod.name]
  final.mod.result$final.sp.bar <- inf$estimation$result.0$theta.bar[final.mod.name]

  final.mod.result <- cbind(final.mod.result,
                            inf$inference$result.1[mstar, c("estimate", "corrected", "lower", "upper")])
  colnames(final.mod.result)[(ncol(final.mod.result)-3):ncol(final.mod.result)] <-
    paste0("final.se.", c("hat", "tilde", "lower", "upper"))

  final.mod.result <- cbind(final.mod.result,
                            inf$inference$result.0[mstar, c("estimate", "corrected", "lower", "upper")])
  colnames(final.mod.result)[(ncol(final.mod.result)-3):ncol(final.mod.result)] <-
    paste0("final.sp.", c("hat", "tilde", "lower", "upper"))

  return(cbind(result, final.mod.result))
}


### FUNCTION: derive_stats
derive_stats <- function(index, name="final.mod", instance, details=FALSE){
  stopifnot(length(index)==1)
  out <- data.frame(name = instance$models[index],
                    train.theta = instance$train.models$theta$theta[index],
                    learn.theta = instance$learn.models$theta$theta[index],
                    thetabar.val = instance$train.models$val$thetahat$theta[index])
  if(details){
    out$train.se = instance$train.models$theta$theta1[index]
    out$learn.se = instance$learn.models$theta$theta1[index]
    out$se.bar.val = instance$train.models$val$thetahat$theta1[index]
    out$train.sp = instance$train.models$theta$theta0[index]
    out$learn.sp = instance$learn.models$theta$theta0[index]
    out$sp.bar.val = instance$train.models$val$thetahat$theta0[index]
  }
  names(out) <- paste(name, names(out), sep=".")
  return(out)
}

### FUNCTION: get_hp
get_hp <- function(fmn, instance){
  # hack for "non-ML" case:
  #if(instance$info["train", "n"] == 0){return(NA)}
  i <- which(sapply(instance$hyperparams, function(x)fmn %in% rownames(x)))
  return(instance$hyperparams[[i]][fmn,])
}
