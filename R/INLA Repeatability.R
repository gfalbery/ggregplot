# INLA Repeatability


Invsqrt <- function(x) {1 / sqrt(x)}


INLARep<-function(model, distribution, variable){
  
  pmtau <- model$marginals.hyperpar$`Precision for the Gaussian observations`
  sigma <- inla.emarginal(Invsqrt, pmtau)
  
  pmtau.Fish <- model$marginals.hyperpar[[paste0("Precision for ",variable)]]
  sigmaFish <- inla.emarginal(Invsqrt, pmtau.Fish)
  c(sigmaFish, sigma)
  
  # From these we can calculate the ICC:

  return(sigmaFish^2 / (sigmaFish^2 + sigma^2))
  
}
