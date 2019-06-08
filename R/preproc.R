### FUNCTION: preproc_shift
#' @importFrom data.table CJ
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @export
preproc_shift <- function(results, vals = seq(-0.05,0.1,0.01)){
  D <- data.table::CJ(job.id=unique(results$job.id), shift=vals)
  R <- dplyr::inner_join(results, D) %>%
    dplyr::mutate(theta0 = opt.theta - shift) %>%
    dplyr::mutate(reject = final.mod.lower > theta0) %>%
    dplyr::mutate(tp = reject & final.mod.learn.theta > theta0) %>%
    dplyr::mutate(fp = reject & final.mod.learn.theta <= theta0)
  return(R)
}

### FUNCTION: aggr_results
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @export
aggr_results <- function(results, type=""){
  results %>%
    dplyr::group_by(shift, select.rule, infer.method, M, n.learn, n.eval) %>%
    dplyr::summarize(rr = mean(reject), tpr=mean(tp), fpr=mean(fp))
}


### FUNCTION: preproc_shift
#' @importFrom data.table CJ
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @export
preproc_shift_cpe <- function(results, vals = seq(-0.05,0.1,0.01), delta=0){
  D <- data.table::CJ(job.id=unique(results$job.id), shift=vals)
  R <- dplyr::inner_join(results, D) %>%
    dplyr::mutate(sp0 = pmin(opt.se.cpe - delta, opt.sp.cpe) - shift) %>% ##### ERR HERE????
    dplyr::mutate(se0 = sp0 + delta) %>%
    dplyr::mutate(reject.sp = final.sp.lower > sp0,
                  reject.se = final.se.lower > se0) %>%
    dplyr::mutate(reject = reject.se & reject.sp) %>%
    dplyr::mutate(tp.sp = reject.sp & final.learn.sp > sp0,
                  tp.se = reject.se & final.learn.se > se0) %>%
    dplyr::mutate(tp = tp.se & tp.sp) %>%
    dplyr::mutate(fp.sp = reject.sp & final.learn.sp <= sp0,
                  fp.se = reject.se & final.learn.se <= se0,
                  fp = reject & (final.learn.sp <= sp0 | final.learn.se <= se0))
  return(R)
}

