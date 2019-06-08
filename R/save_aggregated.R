### FUNCTION: save_aggregated
#' @export
save_aggregated <- function(path = paste0(out.dir, "/MLE_SIM_THEORY_final.theta.mean_BY_S.csv"),
                            matched = findExperiments(algo.name="aggregate_by",
                                                      algo.pars= var=="final.theta.mean" & by =="S", reg=reg),
                            return=FALSE, # should DF be returned?
                            subs=FALSE){  # test with subset of size 1000
  matched <- matched[1:ifelse(subs, 1000, nrow(matched)), ]
  JP.matched <- unwrap(getJobPars(matched, reg=reg))
  D <- unwrap(reduceResultsDataTable(matched, reg=reg))

  R <- cbind(JP.matched %>% dplyr::select(load.id, methods, M, M.start, M.probs, eval.n), D)
  L <- JP.data %>%
    dplyr::rename(load.id = job.id) %>%
    dplyr::select(load.id, n.learn, ratio.lv, red, rho, scenario)

  #stopifnot(all( unique(L$load.id) == unique(R$load.id) ))
  DD <- dplyr::right_join(L, R)

  readr::write_csv(DD, path=path)
  message(paste0("The resulting table has ", nrow(DD), " rows and ", ncol(DD), " columns. Its size is ", round(object.size(DD)*10^-6, 1), " MB."))
  if(return){
    return(DD)
  }else{
    return(NULL)
  }
}
