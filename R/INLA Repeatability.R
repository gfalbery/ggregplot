
# INLA ICC function ####

library(tidyverse)

INLARep <- function(Model, Family = "gaussian"){

  MySqrt <- function(x) {1 / sqrt(x)}

  SigmaList <- list()

  Parameters <- which(names(Model$marginals.hyperpar) %>%substr(1,9)%in%c("Precision","precision"))
  HyperPars <- Model$marginals.hyperpar[Parameters]

  for(x in 1:(length(HyperPars))){
    tau <- HyperPars[[x]]
    sigma <- inla.emarginal(MySqrt, tau)
    sigma <- inla.emarginal(function(x) 1/x, tau)
    SigmaList[[x]] <- sigma
  }

  names(SigmaList) <- names(HyperPars)

  lapply(SigmaList, function(a) a^2/sum(sapply(SigmaList, function(b) b^2)))

}

INLARepPlot <- function(ModelList){

  OutputList <- sapply(ModelList, INLARep) %>% bind_rows %>% data.frame
  OutputList$Model <- as.numeric(rownames(OutputList))

  OutputLong <- OutputList %>% gather(Var, Variance, -"Model") %>%
    mutate(Var = factor(substr(as.character(Var), 15, nchar(as.character(Var))),
                        levels = unique(substr(as.character(Var), 15, nchar(as.character(Var))))))

  ggplot(OutputLong, aes(factor(Model), Variance, fill = Var)) +
    geom_col(position = "stack") +
    labs(x = "Model")

}
