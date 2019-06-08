### FUNCTION: save_results
#' @export
save_results <- function(reg,
                         JP.data,
                         target,
                         return=FALSE,
                         type="results" ){
  excl <- switch(type, result = 1:6, power=1:5)
  R <- switch(type,
              results = unwrap(reduceResultsDataTable(findDone(reg=reg), function(x){x$result}, reg=reg)),
              power = unwrap(reduceResultsDataTable(findDone(reg=reg), reg=reg)))
  JP <- unwrap(getJobPars(findDone(reg=reg), reg=reg))

  stopifnot(all(R$job.id == JP$job.id) & nrow(R) == nrow(JP))

  if(is.character(JP.data)){
    JP.data <- readr::read_csv(JP.data)
  }
  JP.data <- JP.data %>%
    dplyr::rename(load.id = job.id) %>%
    dplyr::select(-methods, -M, -write, -filename, -subfolder, -folder, -sep)

  DD <- dplyr::right_join(JP.data, cbind(JP %>% dplyr::select(-excl), R))

  readr::write_csv(DD, paste0(target))

  message(paste0("The resulting table has ", nrow(DD), " rows and ", ncol(DD), " columns. Its size is ", round(object.size(DD)*10^-6, 1), " MB."))
  if(return){
    return(DD)
  }else{
    return(NULL)
  }
}
