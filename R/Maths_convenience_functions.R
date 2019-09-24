# Mathematical convenience functions ####

Prev <- function(x){

  length(x[x>0])/length(x)

}

logistic <- function(a) exp(a)/(1 + exp(a))
logit <- function(a) log(a/(1-a))
