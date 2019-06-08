### FUNCTION: correct_estimate
correct_estimate <- function(theta.compl, setS, correct.method="none"){
  switch(correct.method,
         none = theta.compl[setS])
}

### FUNCTION: estimate_power
estimate_power <- function(theta0, theta, sigma, n, n.eval, alpha, alternative="greater", method="conditional"){
  theta0 <- theta0[rep(1, length(theta))]

  R <- cov2cor(sigma)
  cv <- mvtnorm::qmvnorm(1-alpha, tail=alt2tail(alternative),
                         mean=rep(0, nrow(R)), sigma=R)$quantile
  tstat <- (theta- theta0)/sqrt(diag(sigma*(n)/n.eval))

  power <-switch(method,
                 conditional = conditional_power(tstat, cv, R, alternative),
                 predictive = NA)
  return(power)
}

### FUNCTION: predictive_power
predictive_power <- function(tstat, cv, R, alternative){
  NA
}

### FUNCTION: conditional_power
conditional_power <- function(tstat, cv, R, alternative){
  u <- rep(cv, length(tstat)); l <- -u
  if(alternative == "greater"){l <- rep(-Inf, length(tstat))}
  if(alternative == "less"){u <- rep(Inf, length(tstat))}
  return(1 - as.numeric(mvtnorm::pmvnorm(lower=l, upper=u, mean=tstat, sigma=R)))
}

### FUNCTION: alt2tail
alt2tail <- function(alternative){
  switch(which(alternative[1] ==  c('greater', 'less', 'two.sided')),
         "lower.tail", "upper.tail", "both.tails")
}
