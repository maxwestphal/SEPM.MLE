### FUNCTION: generate_hyperparams
generate_hyperparams <- function(method="glmnet",
                                 M=10,
                                 tuning="random",
                                 x=NULL,
                                 y=NULL,
                                 ...){
  do.call(what = paste("generate_hyperparams", method, sep="_"),
          args = list(len=M, tuning=tuning, x=x, y=y))
}

### FUNCTION: sample.hyperparams
sample_hyperparams <- function(M=5,
                               ...){
  args <- list(...)
  do.call(cbind.data.frame, lapply(args, sample, size=M, replace=TRUE))
}

### FUNCTION: generate_hyperparams_glmnet
### Ref: https://github.com/topepo/caret/blob/master/models/files/glmnet.R
#' @importFrom glmnet glmnet
generate_hyperparams_glmnet <- function(len=10,
                                        tuning="random",
                                        x=NULL,
                                        y=NULL){
  if(tuning == "grid"){
    init <- glmnet::glmnet(Matrix::as.matrix(x), y,
                           family = "binomial",
                           nlambda = ceiling(sqrt(len))+2,
                           alpha = .5)
    lambda <- unique(init$lambda)
    lambda <- lambda[-c(1, length(lambda))]
    lambda <- lambda[1:min(length(lambda), ceiling(sqrt(len)))]
  }
  switch(tuning,
         grid   = expand.grid(alpha = seq(1, 0.1, length.out = ceiling(sqrt(len))),
                              lambda = lambda)[1:len, ],
         random = data.frame(alpha = runif(len, min = 0, 1),
                             lambda = 2^runif(len, min = -10, 3)))
}

### FUNCTION: generate_hyperparams_ranger
### Ref: https://github.com/topepo/caret/blob/master/models/files/ranger.R
generate_hyperparams_ranger <- function(len=10,
                                        tuning="random",
                                        x=NULL,
                                        y=NULL){
  srules <- c("gini", "extratrees")
  switch(tuning,
         grid = expand.grid(splitrule = srules,
                            min.node.size = 1,
                            mtry  = round(seq(ncol(args$x), 2, length.out=len/2)))[1:len, ],
         random = data.frame(
           min.node.size= sample(1:(min(20,nrow(x))), size = len, replace = TRUE),
           mtry = sample(1:ncol(x), size = len, replace = TRUE),
           splitrule = sample(srules, size = len, replace = TRUE)))
}


### FUNCTION: generate_hyperparams_xgbTree
### (Reference: https://github.com/topepo/caret/blob/master/models/files/xgbTree.R)
generate_hyperparams_xgbTree <- function(len=10,
                                         tuning="random",
                                         x=NULL,
                                         y=NULL){
  switch(tuning,
         grid = expand.grid(max_depth = seq(1, len),
                            nrounds = floor((1:len) * 50),
                            eta = c(.3, .4),
                            gamma = 0,
                            colsample_bytree = c(.6, .8),
                            min_child_weight = c(1),
                            subsample = seq(.5, 1, length = len)),
         random = data.frame(nrounds = sample(1:1000, size = len, replace = TRUE),
                             max_depth = sample(1:10, replace = TRUE, size = len),
                             eta = runif(len, min = .001, max = .6),
                             gamma = runif(len, min = 0, max = 10),
                             colsample_bytree = runif(len, min = .3, max = .7),
                             min_child_weight = sample(0:20, size = len, replace = TRUE),
                             subsample = runif(len, min = .25, max = 1)))
}

### FUNCTION: generate_hyperparams_rpartCost
### Ref: https://github.com/topepo/caret/blob/master/models/files/rpartCost.R
### (Cost is apparantly the cost of a false positive, hence this should be < 1 if prev < 0.5)
generate_hyperparams_rpartCost <- function(len=10,
                                           tuning="random",
                                           x=NULL,
                                           y=NULL){
  switch(tuning,
         grid = expand.grid(cp=10^seq(-5, -1, 1),
                            Cost = 2^seq(-4,4, length.out=len/5)),
         random = data.frame(cp = 10^runif(len, min = -8, max = -1),
                             Cost = 2^runif(len, -1/mean(y), 1/(1-mean(y)))))
}

### FUNCTION: generate_hyperparams_svmLinearWeights2
### Ref: https://github.com/topepo/caret/blob/master/models/files/svmLinearWeights2.R
### (weight = runif(len, min = 1, max = 25) --> seems way off for low prevalance of Y=1)
generate_hyperparams_svmLinearWeights2 <- function(len=10,
                                                   tuning="random",
                                                   x=NULL,
                                                   y=NULL){
  switch(tuning,
         grid = expand.grid(cost = 2 ^((1:len) - 3),
                            Loss = c("L1", "L2"),
                            weight = 1:len),
         random = data.frame(cost = 2^runif(len, min = -10, max = 10),
                             Loss = sample(c("L1", "L2"), size = len, replace = TRUE),
                             weight = 2^runif(len, -1/(1-mean(y)), 1/(mean(y)))))
}




