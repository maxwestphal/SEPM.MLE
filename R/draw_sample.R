### FUNCTION: draw_sample
#' @importFrom mvtnorm rmvnorm
#' @export
draw_sample <- function(n = 100,
                        score = score_linearsparse,
                        pars = list(P=50, Prel=5, rho=0, red=0, b=0.5, s=1, mu=4),
                        seed = NULL,
                        tune = FALSE){
  set.seed(seed)
  # create correlation matrix and feature data:
  R <- construct_corrmat(P=pars$P, Prel=pars$Prel, rho=pars$rho, red=pars$red)
  x <- mvtnorm::rmvnorm(n=n, mean=rep(0, pars$P), sigma=R)
  colnames(x) <- paste0("X", 1:pars$P)

  # derive score and label data:
  nu <- do.call(score, list(x=x, pars=pars))*pars$s + pars$m
  y <- rbinom(n, 1, sigmoid(nu))

  # use tune=TRUE to assess scenario properties:
  if(tune){
    hist(sigmoid(nu), xlim=c(0,1), freq=FALSE)
    y2 <- as.numeric(sigmoid(nu)>0.5)
    #y2 <- rbinom(n, 1, sigmoid(nu))
    result <- data.frame(prev=mean(y), acc=mean(y==y2),
                          sens=sum(y==1 & y2==1)/sum(y==1),
                          spec=sum(y==0 & y2==0)/sum(y==0))
    return(result)
  }
  return(list(Y=y, X=x))
}
