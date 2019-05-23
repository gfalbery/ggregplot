

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

  NameList <- sapply(names(HyperPars), function(a) last(strsplit(a, " ")[[1]]))

  substr(NameList, 1, 1) <- toupper(substr(NameList, 1, 1))

  if(any(NameList=="Observations")) NameList[NameList=="Observations"] <- "Residual"

  names(SigmaList) <- NameList

  if(any(names(Model$marginals.hyperpar) %>%substr(1,5)=="Range")){

    Var <- names(Model$marginals.hyperpar)[which(names(Model$marginals.hyperpar) %>%substr(1,5)=="Range")]
    Expl <- Var %>% str_split(" ") %>% last %>% last

    i1 = inla.spde.result(Model, Expl, spde)
    i2 = i1$marginals.tau[[1]] %>% inla.tmarginal(function(a) 1/a, .) %>% inla.mmarginal()

    SigmaList$SPDE <- i2

  }

  if(Family == "binomial"){
    lapply(SigmaList, function(a) (a)/c(sum(sapply(SigmaList, function(b) b))+pi^(2/3))) %>% return
  } else{

    if(Family == "gaussian") lapply(SigmaList, function(a) a/(sum(unlist(SigmaList)))) %>% return
    #if(Family == "gaussian") lapply(SigmaList, function(a) a) %>% return
  }
}

INLARepPlot <- function(ModelList, ModelNames = NULL, Just = F, DICOrder = F, Family = "gaussian", Residual = T){

  OutputList <- sapply(ModelList, function(a) INLARep(a, Family = Family)) %>% bind_rows %>% data.frame
  OutputList$Model <- as.numeric(rownames(OutputList)) %>% as.factor

  if(!is.null(ModelNames)){
    levels(OutputList$Model) <- ModelNames
  }

  if(DICOrder) OutputList <- OutputList[rev(order(sapply(ModelList, MDIC))),]; OutputList$Model <- factor(OutputList$Model, levels = unique(OutputList$Model))

  OutputLong <- OutputList %>% gather(Var, Variance, -"Model") %>%
    mutate(Var = factor(Var, levels = unique(Var))) %>% na.omit()

  if(!Residual){
    OutputLong <- OutputLong %>% filter(!Var == "Residual")
  }

  if(Just){ Angle = 45; Hjust = 1 }else{ Angle = 0; Hjust = 0.5}

  ggplot(OutputLong, aes(factor(Model), Variance, fill = Var)) +
    geom_col(position = "stack") +
    labs(x = "Model") +
    theme(axis.text.x = element_text(angle = Angle, hjust = Hjust)) +
    lims(y = c(0,1))

}
