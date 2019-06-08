### FUNCTION: normalize
normalize <- function(x){
  (x - mean(x))/sd(x)
}

### FUNCTION: logit
logit <- function(x){
  log(x/(1-x))
}

### FUNCTION: sigmoid
sigmoid <- function(x){
  1/(1+exp(-x))
}

### FUNCTION: construct_corrmat
construct_corrmat <- function(P=15,
                              Prel=5,
                              rho = 0.5,
                              red=1){
  Pred <- Prel * red
  stopifnot(Prel + Pred <= P)
  R <- diag(rep(1,P))
  prel <- 1:Prel
  IJ <- as.matrix(do.call(rbind, lapply(prel, function(x){p = seq(x, x+Pred, Prel); expand.grid(i=p, j=p)})))
  R[IJ[IJ[,1] != IJ[,2], ]] <- rho
  R
}

