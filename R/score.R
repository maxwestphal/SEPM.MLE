### FUNCTION: score_friedman1 (scenarios "C" and "D")
score_friedman1 <- function(x, pars=list()){
  10 * sin(pi * x[, 1] * x[, 2]) + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
}

### FUNCTION: score_friedman2
score_friedman2 <- function(x, pars=list()){
  sqrt(x[, 1]^2 + (x[, 2] * x[, 3] - 1/(x[, 2] * x[, 4]))^2)
}

### FUNCTION: score_friedman3
score_friedman3 <- function(x, pars=list()){
  atan((x[, 2] * x[, 3] - 1/(x[, 2] * x[, 4]))/x[, 1])
}

### FUNCTION: score_friedman12
score_friedman12 <- function(x, pars=list()){
  normalize(b[1] * score_friedman1(x[, 1:5]) + b[2] * score_friedman2(x[, 6:9]))
}

### FUNCTION: score_friedman13 (scenarios "E" and "F")
score_friedman13 <- function(x, pars=list(w1=1, w3=1)){
  pars$w1 * score_friedman1(x[, 1:5]) + pars$w3 * score_friedman3(x[, 6:9])
}

### FUNCTION: score_friedman23
score_friedman23 <- function(x, pars=list(w2=1, w3=1)){
  pars$w2 * score_friedman2(x[, 1:4]) + pars$w3 * score_friedman3(x[, 5:8])
}

### FUNCTION: score_linear
score_linear <- function(x, beta, intercept=0){
  intercept + x%*%beta
}

### FUNCTION: score_linearsparse (EOMPM scenario "A")
score_linearsparse <- function(x, pars=list(P=50, Prel=5, mu=4)){
  beta <- c(rep(pars$mu, pars$Prel), rep(0, pars$P-pars$Prel))
  score_linear(x, beta, ifelse(is.null(pars$intercept), 0, pars$intercept))
}

### FUNCTION: score_lineardense (EOMPM scenario "B")
score_lineardense <- function(x, pars=list(P=50, Prel=50, mu=6)){
  beta <- pars$mu*c((-1)^(1:pars$Prel-1)*1/(1:pars$Prel), rep(0, pars$P-pars$Prel))
  score_linear(x, beta, ifelse(is.null(pars$intercept), 0, pars$intercept))
}


### GLOBAL OBJECT: SCENARIOS
SCENARIOS <- list()
SCENARIOS[["EOMPM_A"]] <-
  list(score = score_linearsparse, pars = list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=4))
SCENARIOS[["EOMPM_B"]] <-
  list(score = score_lineardense, pars = list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=6))

SCENARIOS[["EOMPM_A2"]] <-
  list(score = score_linearsparse, pars = list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=2))
SCENARIOS[["EOMPM_B2"]] <-
  list(score = score_lineardense, pars = list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=3))

SCENARIOS[["MLE_SIM_F1_prev30"]] <-
  list(score = score_friedman1, pars = list(P=50, Prel=5, rho=0, red=0, m=2.5, s=-0.5))
SCENARIOS[["MLE_SIM_F1_prev15"]] <-
  list(score = score_friedman1, pars = list(P=50, Prel=5, rho=0, red=0, m=-2.5, s=-0.55))

SCENARIOS[["MLE_SIM_F3_prev30"]] <-
  list(score = score_friedman3, pars = list(P=50, Prel=4, rho=0, red=0, m=-15, s=-12))
SCENARIOS[["MLE_SIM_F3_prev15"]] <-
  list(score = score_friedman3, pars = list(P=50, Prel=4, rho=0, red=0, m=-50.75, s=-34))

SCENARIOS[["MLE_SIM_F13_prev30"]] <-
  list(score = score_friedman13, pars = list(P=50, Prel=9, rho=0, red=0, m=2.5, s=-0.5, w1=1, w3=-1))
SCENARIOS[["MLE_SIM_F13_prev15"]] <-
  list(score = score_friedman13, pars = list(P=50, Prel=9, rho=0, red=0, m=-2.5, s=-1, w1=0.5, w3=-0.75))



# TESTS: tune problems to yield realistic optimal performance ~ 85%-95%
# (=performance of true data-generating model)
# N <- 100000
# ## EOMPM A/B:
# draw_sample(n=N, score = score_linearsparse,
#             pars=list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=2), seed=1, tune=T)
# draw_sample(n=N, score = score_lineardense,
#             pars=list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=3), seed=1, tune=T)
#
# ## Friedman1:
# draw_sample(n=N, score = score_friedman1,
#             pars=list(P=50, Prel=5, rho=0, red=0, m=2.5, s=-0.5), seed=1, tune=T)
# draw_sample(n=N, score = score_friedman1,
#             pars=list(P=50, Prel=5, rho=0, red=0, m=-2.5, s=-0.55), seed=1, tune=T)
#
# ## Friedman2: not suitable
#
# ## Friedman3:
# draw_sample(n=N, score = score_friedman3,
#             pars=list(P=50, Prel=4, rho=0, red=0, m=-15, s=-12), seed=1, tune=T)
# draw_sample(n=N, score = score_friedman3,
#             pars=list(P=50, Prel=4, rho=0, red=0, m=-50.75, s=-34), seed=1, tune=T)
#
#
# ## Friedman13:
# draw_sample(n=N, score = score_friedman13,
#             pars=list(P=50, Prel=9, rho=0, red=0, m=2.5, s=-0.5, w1=1, w3=-1), seed=1, tune=T)
# draw_sample(n=N, score = score_friedman13,
#             pars=list(P=50, Prel=9, rho=0, red=0, m=-2.5, s=-1, w1=0.5, w3=-0.75), seed=1, tune=T)







