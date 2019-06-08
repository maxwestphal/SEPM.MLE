### FUNCTION: load_data
#' @export
load_data <- function(folder = "G:/R/MLE_SIM/DATA",
                      base = "job", id=1){
  load(paste0(folder, "/", base, id, ".RData"))
  return(instance)
}

