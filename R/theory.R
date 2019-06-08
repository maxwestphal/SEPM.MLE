### FUNCTION: study_theory
#' @export
theory <- function(load.folder = "E:/DataScience/R/SIM/MLE_SIM/DATA",
                   load.base = "job",
                   load.id = 1337,
                   methods = NA,
                   M = 200,
                   M.start = NA,
                   M.probs = "uniform",
                   M.seed = 1,
                   eval.first = 1,
                   eval.n = 100,
                   eval.rdm = FALSE,
                   data = NULL,
                   job = NULL){

  ### LOAD DATA:
  instance <- load_data(load.folder, load.base, load.id)

  # subsample models:
  instance <- sample_setM(instance=instance, methods=methods,
                          M=M, M.start=M.start, M.probs=M.probs, M.seed=M.seed)

  # get relevant observations
  obs <- get_obs(eval.rdm, eval.first, eval.n)
  thetahat.eval <- colMeans(as.matrix(instance$learn.models$eval$pred ==
                                        instance$learn.models$eval$labels)[obs, ])

  # cases:
  #cases <- c("overall", as.character(unique(D$method)))

  # build data.frame
  D <- data.frame(model = instance$models,
                  method = gsub('[0-9]+', '', instance$models),
                  train.acc.train = instance$train.models$train$thetahat$theta,
                  train.acc.val   = instance$train.models$val$thetahat$theta,
                  train.acc.pop   = instance$train.models$theta$theta,
                  learn.acc.learn = instance$learn.models$learn$thetahat$theta,
                  learn.acc.eval  = thetahat.eval,
                  learn.acc.pop   = instance$learn.models$theta$theta)

  D <- dplyr::mutate(D, gamma = learn.acc.pop - train.acc.pop,
                     train.eps.train = train.acc.train - train.acc.pop,
                     train.eps.val   = train.acc.val   - train.acc.pop,
                     learn.eps.learn = learn.acc.learn - learn.acc.pop,
                     learn.eps.eval  = learn.acc.eval  - learn.acc.pop,
                     val.rank.ave = rank(-train.acc.val, ties.method = "average"),
                     val.rank.rdm = rank(-train.acc.val, ties.method = "random"))

  #C <- theta_curve(D=D, n.val=instance$info["val", "n"])

  #plot(C$S, C$final.theta.mean, type="l")
  #lines(C$S, C$final.theta.rdm, col="red")
  #lines(C$S, C$final.theta.first, col="blue")

  return(list(data = D, n.val=instance$info["val", "n"]))
}


### FUNCTION: aggregate_by
#' @importFrom dplyr between
#' @export
aggregate_by <- function(instance,
                         var,
                         by,
                         breaks,
                         slot="data",
                         query="TRUE",
                         out.char="",
                         aggr = mean,
                         data=NULL,
                         job=NULL){
  dat <- switch(slot, data=instance$data, theta.curve=theta_curve(instance$data, instance$n.val))
  if(is.character(breaks)){
    breaks <- as.numeric(strsplit(breaks, ",")[[1]])
  }
  I <- 1:(length(breaks)-1)
  breaks <- sort(breaks)
  dat <- dplyr::mutate(dat, y = eval(parse(text=var)), x=eval(parse(text=by)))
  dat <- dplyr::filter(dat, eval(parse(text=query)))
  out <- as.data.frame(t(sapply(I, function(i){
    aggr(dat[dplyr::between(dat[,"x"], breaks[i], breaks[i+1]-1e-12), "y"])})))
  names(out) <- sapply(I, function(i){paste0(out.char, "[", breaks[i], ",", breaks[i+1], ")")})
  out <- cbind(data.frame(var=var, by=by,
                          max.learn.acc=max(instance$data$learn.acc.pop),
                          max.train.acc=max(instance$data$train.acc.pop),
                          max.gamma = max(instance$data$gamma)), out)
  return(out)
}

### FUNCTION: theta_curve
#' @importFrom dplyr arrange
theta_curve <- function(D,
                        n.val=instance$info["val", "n"]){
  D <- D %>% dplyr::arrange(val.rank.rdm)
  thetahat1 <- D$train.acc.val[1]; sehat1 <- sqrt(thetahat1*(1-thetahat1)/n.val)
  S <- 1:nrow(D)

  mstar.all <- lapply(S, function(r) all_max(D[1:r,"learn.acc.eval"]))
  mstar.rdm <- sapply(mstar.all, function(x) sample(x, 1))

  out <- data.frame(S=S,
                    mstar.rdm = mstar.rdm,
                    mstar.count = sapply(mstar.all, length),
                    max.theta = max(D$learn.acc.pop),
                    max.theta.S = sapply(S, function(x) max(D[1:x, "learn.acc.pop"])),
                    final.theta.nodata = sapply(S, function(x) mean(D[1:x, "learn.acc.pop"])),
                    final.theta.mean  = sapply(mstar.all, function(x) mean(D[x, "learn.acc.pop"])),
                    final.theta.rdm   = sapply(mstar.rdm, function(x) D[x, "learn.acc.pop"]),
                    final.theta.first = sapply(mstar.all, function(x) D[x[1], "learn.acc.pop"]),
                    final.thetahat.mean  = sapply(mstar.all, function(x) mean(D[x, "learn.acc.eval"])),
                    final.thetahat.rdm   = sapply(mstar.rdm, function(x) D[x, "learn.acc.eval"]),
                    final.thetahat.first = sapply(mstar.all, function(x) D[x[1], "learn.acc.eval"]),
                    k.se = sapply(S, function(x) (thetahat1 - D[x, "train.acc.val"])/sehat1))
  return(out)
}

