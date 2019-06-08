#' Select models based on validation performance and conduct an evaluation study for co-primary endpoints
#' sensitivity (se) and specificity (sp).
#'
#' This function reads in data instances produces via generate_data() and emulates the process of
#' conducting an evaluation study for one or multiple selected prediction models.
#'
#' @param load.folder character, path containing data instances
#' @param load.base character, define names of data instances (default="job")
#' @param load.id integer, the data instance with name paste0(load.base, load.id, ".RData")) is loaded
#' from directory load.folder
#' @param methods character, potentially subset available prediction models by method (=learning algorithm)
#' e.g. recover elastic net models by specifying methods="glmnet" (caret train.method),
#' no effect if methods=NA (default)
#' @param M integer, number of models to subsample from available models (restricted via methods argument),
#' needs to be less or euqal than number of available models (200 per default)
#' @param M.start integer, starting index for subsetting
#' @param M.probs character, "uniform" for random subset, "learn.theta" for P(selected)=learn.theta(=true model performance),
#' "learn.theta.neg" for P(selected)=1-learn.theta
#' @param M.seed integer, seed for random subsetting (i.e. if M.probs != "uniform")
#' @param eval.first integer, index of first evaluation observation (from all available)
#' @param eval.n integer, test sample size
#' @param eval.rdm logical, choose test samples randomly? (default: FALSE)
#' @param estimate.method character, estimation method in SEPM package default ("beta.approx")
#' @param estimate.args character, specify additional estimation argument as character of form
#' "arg1=value1_arg2=value2_..."
#' @param select.method character, selection method based on validation ranking, e.g. "rank" (default) or "se"
#' @param select.args character, further arguments defining selection rule e.g. "r=1" for
#' select.method="rank" to choose only best validation models or "c=1" for select.method="se"
#' (which defines the 'within1SE# rule)
#' @param select.limit integer, maximum number of models to evaluate
#' @param known.delta logical, selection based on validation performance + known performance gain after re-training?
#' (default: FALSE)
#' @param infer.method character, defines the statistical test, e.g. "maxT", "Bonferroni" or "naive"
#' @param alternative charatcer, either "greater" (default), "lower" or "two.sided"
#' @param alpha numeric, significan level (default: 0.025)
#' @param transform character, specifies transformation of test statistics, passed to SEPM::evaluate()
#' @param data NULL, batchtools argument
#' @param job NULL, batchtools argument
#'
#' @return Returns a list which contains all relevant characteristics of the evaluation study.
#'
#' @export
study_sensspec <- function(load.folder = "E:/DataScience/R/SIM/MLE_SIM/DATA",
                           load.base = "job",
                           load.id = 8,
                           methods = NA,
                           M = 200,
                           M.start = NA,
                           M.probs = "uniform",
                           M.seed = 1,
                           eval.first = 1,
                           eval.n = 200,
                           eval.rdm = FALSE,
                           estimate.method = "beta.approx",
                           estimate.args = "",
                           select.method = "weighted.acc",
                           select.args = "c=1_w0=0.5_w1=0.5",
                           select.limit = "sqrt",
                           known.delta = FALSE,
                           sesp.delta = 0,
                           infer.method = "maxT",
                           alternative = "greater",
                           alpha = 0.025,
                           transform = "none",
                           data = NULL,
                           instance = NULL,
                           job = NULL){
  ### TESTING:
  # load.folder = "E:/DataScience/R/SIM/MLE_SIM/DATA"; load.base = "job"; load.id = 8; methods = NA; M = 200; M.start = NA; M.probs = "uniform"; M.seed = 1
  # eval.first = 1; eval.n = 200; eval.rdm = FALSE; estimate.method = "beta.approx"; estimate.args = ""; select.method = "weighted.acc"; select.args = "c=01_w0=0.5_w1=0.5";
  # select.limit = "sqrt"; known.delta = FALSE; infer.method = "maxT"; alternative = "greater"; alpha = 0.025; transform = "none"; sesp.delta = 0
  ###

  ### LOAD DATA:
  ### LOAD DATA:
  if(is.null(instance)){
    instance <- load_data(load.folder, load.base, load.id)
  }

  # subsample models:
  instance <- sample_setM(instance=instance, methods=methods,
                          M=M, M.start=M.start, M.probs=M.probs, M.seed=M.seed)

  ### LEARNING PHASE
  hyp <- SEPM::define_hypothesis(target="accuracy.cp", threshold=c(0.5, 0.5),
                                 alternative=alternative, alpha=alpha)

  # get eval observations:
  obs.eval <- get_obs(eval.rdm, eval.first, eval.n, n.max=instance$info["eval", "n"])

  # select.rule / select.args preproccessing:
  ps <- preproc_select(select.method, select.args, select.limit, instance, eval.n)
  select.method <- ps$sm; select.args <- ps$sa; average <- ps$av
  if(known.delta){select.args$delta = ps$delta}


  # model selection
  sel <- SEPM::learn(hypothesis = hyp,
                     predictions = as.matrix(instance$train.models$val$pred),
                     labels=instance$train.models$val$labels,
                     estimate = estimate.method,
                     estimate.args = string2list(estimate.args, convert=as.numeric),
                     select=select.method, select.args=select.args,
                     weights = 1, n.test = NA, seed=1, messages=FALSE)
  setS <- sel$selection$result$selected

  ### EVALUATION PHASE:

  # get predictions / average for select.method=="ensemble"
  pred <- as.matrix(instance$learn.models$eval$pred[obs.eval, setS, drop=F])
  if(average){
    pred <- matrix(round(rowMeans(pred))); colnames(pred) <- "ensemble"
  }

  # statistical inference:
  y <- instance$learn.models$eval$label[obs.eval]
  inf <- SEPM::evaluate(hypothesis = hyp,
                        predictions = pred,
                        labels=y,
                        estimate = estimate.method, estimate.args = NULL,
                        infer=infer.method, transform = transform, seed=1, messages=FALSE)

  result <- derive_result_sesp(load.id=load.id, instance=instance, pred=pred, inf=inf, delta=sesp.delta)

  info <- instance$info
  info["eval", c("n", "n1", "prev")] <- c(eval.n, sum(y), sum(y)/eval.n)

  return(list(job = list(id=job$job.id, seed=job$seed, pars=job$pars),
              info = info,
              selection = sel$selection$result$model.name[setS],
              final.hp = get_hp(fmn=as.character(result$final.name), instance),
              result = result))
}
