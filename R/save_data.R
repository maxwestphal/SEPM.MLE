### FUNCTION: save_data
#' @export
save_data <- function(instance,
                      write = TRUE,
                      filename = "job",
                      subfolder="MLE_SIM",
                      folder=getwd(),
                      sep="/",
                      data=NULL,
                      job=NULL){
  if(write){
    filename <- paste0(filename, max(0, instance$job$id))
    dir <- paste(folder, subfolder, sep=sep)
    dir.create(dir)
    save(instance, file=paste(dir, paste0(filename, ".RData"), sep=sep))
  }
  return(instance)
}



