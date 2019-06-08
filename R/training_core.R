### FUNCTION: fit_models
fit_models <- function(method,
                       hp,
                       sample,
                       metric="Accuracy"){
  lapply(1:nrow(hp), function(i) fit_model(method, hp=hp[i,], sample=sample, metric))
}

### FUNCTION: fit_model
fit_model <- function(method,
                      hp,
                      sample,
                      metric="Accuracy"){
  ctrl <- caret::trainControl(method = "none",
                              search = "grid",
                              preProc = c("center", "scale"),
                              verboseIter = F,
                              returnResamp = "all")

  model <- caret::train(x = sample$X,
                        y = factor(sample$Y),
                        method = method,
                        tuneGrid = hp,
                        trControl = ctrl,
                        metric = metric)
  return(model)
}

### FUNCTION: sample2df
sample2df <- function(sample){
  data.frame(Y=factor(sample$Y), sample$X)
}

### FUNCTION:
fac2num <- function(x){
  as.numeric(as.character(x))
}
