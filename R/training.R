### Function: training
training <- function(method="glmnet",
                     M=10,
                     tuning = "grid",
                     samples){
  stopifnot(length(method) == length(M))
  hp <- models.train <- models.learn <- list()
  for(k in 1:length(method)){
    hp[[method[k]]] <- generate_hyperparams(method=method[k], M=M[k], tuning=tuning,
                                            x=samples$train$X, y=samples$train$Y)
    rownames(hp[[method[k]]]) <- paste0(method[k], 1:M[k])
    models.train[[method[k]]] <- fit_models(method=method[k], hp=hp[[method[k]]],
                                            sample=samples$train)
    models.learn[[method[k]]] <- fit_models(method=method[k], hp=hp[[method[k]]],
                                            sample=samples$learn)
  }
  model.list <- list(train.models=unlist(models.train, recursive=FALSE),
                     learn.models=unlist(models.learn, recursive=FALSE),
                     hp=hp)
  return(model.list)
}




