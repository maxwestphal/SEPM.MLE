#' @export
#' @importFrom data.table data.table
extract <- function(instance, what="theta", var="theta", choice="learn.models",
                    q=c(0.5, 0.75, 0.95, 1), theta0 = c(0.75, 0.8, 0.85, 0.9, 0.95), delta=0){
  if(what=="theta" & !is.null(q)){
    return(data.table::data.table(t(quantile(instance[[choice]][[what]][, var], q))))
  }
  if(what=="theta" & !is.null(theta0)){
    out <- data.table::data.table(t(sapply(theta0, function(d)
      mean(instance[[choice]][[what]][, "theta1"] > d+delta &
             instance[[choice]][[what]][, "theta0"] > d))))
    names(out) <- paste0("co", theta0*100)
    return(out)
  }
}

# instance <- generate_data(P=50, M=2)
#
# str(instance)
# instance$hyperparams$glmnet["glmnet1",]
#
# instance$learn.models$theta
# extract(instance, q=NULL, theta0=c(-0.01,0), delta=-10)

