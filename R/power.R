### FUNCTION: power_accuracy
#' @export
power_accuracy <- function(load.folder = "D:/SIM/MLE_SIM/DATA/DATA",
                           load.base = "job",
                           load.id = 8,
                           methods = NA,
                           M = 200,
                           M.start = 1,
                           M.probs = "uniform",
                           M.seed = 1,
                           eval.n = 200,
                           delta = 0.05,
                           select.method ="se",
                           select.args ="c=1",
                           select.limit = "sqrt",
                           estimate.method = "beta.approx",
                           estimate.args = "",
                           known.gamma = FALSE,
                           alpha = 0.025,
                           alternative = "greater",
                           power.method = "conditional",
                           correct.method = "none",
                           data = NULL,
                           job = NULL){
  n.eval <- eval.n
  ### LOAD DATA:
  instance <- load_data(load.folder, load.base, load.id)

  # subsample models:
  instance <- sample_setM(instance=instance, methods=methods,
                          M=M, M.start=M.start, M.probs=M.probs, M.seed=M.seed)

  ### LEARNING PHASE
  hyp <- SEPM::define_hypothesis(target="accuracy", threshold=0.5,
                                 alternative=alternative, alpha=alpha)

  # get eval observations:
  # obs.eval <- get_obs(eval.rdm, eval.first, eval.n)

  # select.rule / select.args preproccessing:
  ps <- preproc_select(select.method, select.args, instance)
  select.method <- ps$sm; select.args <- ps$sa; average <- ps$av
  if(known.gamma){select.args$delta = ps$delta}
  if(select.limit == "sqrt"){select.args$select.max = round(sqrt(eval.n))}

  # model selection:
  sel <- SEPM::learn(hypothesis = hyp,
                     predictions = as.matrix(instance$train.models$val$pred),
                     labels=instance$train.models$val$labels,
                     estimate = estimate.method,
                     estimate.args = string2list(estimate.args, convert=as.numeric),
                     select=select.method, select.args=select.args,
                     weights = 1, n.test = NA, seed=1, messages=FALSE)
  setS <- sel$selection$result$selected


  ### POWER ESTIMATION:
  theta <- correct_estimate(sel$estimation$result$theta.hat, setS, correct.method)

  theta.max <- max(instance$learn.models$theta$theta)
  theta0 <- theta.max - delta

  pow <- estimate_power(theta0 = theta0,
                        theta = theta,
                        sigma = sel$estimation$result$sigma.hat[setS, setS, drop=FALSE],
                        n = sel$estimation$result$n,
                        n.eval = n.eval,
                        alpha = alpha,
                        alternative=alternative,
                        method=power.method)

  return(data.frame(power=pow, theta0=theta0, S.ctrl=sum(setS),
                    V=sum(instance$learn.models$theta$theta[setS]>theta0),
                    best.val.mod = names(which.max(sel$estimation$result$theta.hat))))

}

#power_accuracy(n.eval=200, load.id=9, select.method = "rank", select.args="r=5")


# TODO: make sure all args are included to sample same models as in study
# TODO: correction: "none" (naive), ADAPT FROM HWANG: "emp.bayes" [Normal-normal version] (1993) for theta ~ beta(alpha, beta);
# TODO: make sure no step is only valid for accuracy (e.g. covariance!)

# theta0 = 0.8
# theta = seq(0.90, 0.8, length.out = 3)
# n = 200
# n.eval= 100
# sigma = diag(theta*(1-theta)/n)
# sigma
# alpha = 0.025
#
# alt2tail(h$alternative)
#
# library(copula)
# set.seed(100)
# ?normalCopula
# myCop <- normalCopula(param=c(0.4,0.2,-0.8), dim = 3, dispstr = "un")
# getSigma(myCop)
# myMvd <- mvdc(copula=myCop, margins=rep("beta", 3),
#               paramMargins=list(list(shape=2, scale=1),
#                                 list(shape1=2, shape2=2),
#                                 list(shape1=2, shape2=3)) )
#
# str(myCop)
#
# ?rMvdc
# Z2 <- rMvdc(2000, mvdc=myMvd)
# colnames(Z2) <- c("x1", "x2", "x3")
# pairs.panels(Z2)







