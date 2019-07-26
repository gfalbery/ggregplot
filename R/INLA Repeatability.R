

# INLA ICC function ####

library(tidyverse)

INLARep <- function(Model, Family = "gaussian"){

  SigmaList <- CIList <- list()

  Parameters <- which(names(Model$marginals.hyperpar) %>%substr(1,9)%in%c("Precision","precision"))
  HyperPars <- Model$marginals.hyperpar[Parameters]

  for(x in 1:(length(HyperPars))){
    tau <- HyperPars[[x]]
    sigma <- inla.emarginal(function(x) 1/x, tau)
    SigmaList[[x]] <- sigma
    ci <- tau %>% inla.tmarginal(function(a) 1/a, .) %>% inla.hpdmarginal(p = 0.95)
    CIList[[x]] <- ci
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

    ci <- i1$marginals.tau[[1]] %>% inla.tmarginal(function(a) 1/a, .) %>% inla.hpdmarginal(p = 0.95)
    CIList$SPDE <- ci

  }

  if(Family == "binomial"){

    ReturnList <- sapply(SigmaList, function(a) (a)/c(sum(sapply(SigmaList, function(b) b))+pi^(2/3)))

    LowerList <- sapply(map(CIList, 1), function(a) (a)/c(sum(sapply(SigmaList, function(b) b))+pi^(2/3)))
    UpperList <- sapply(map(CIList, 2), function(a) (a)/c(sum(sapply(SigmaList, function(b) b))+pi^(2/3)))

  }

  if(Family == "gaussian"){

    ReturnList <- sapply(SigmaList, function(a) a/(sum(unlist(SigmaList))))

    LowerList <- sapply(map(CIList, 1), function(a) (a)/c(sum(unlist(SigmaList))))
    UpperList <- sapply(map(CIList, 2), function(a) (a)/c(sum(unlist(SigmaList))))

  }

  ReturnDF <- data.frame(
    Mean = ReturnList,
    Lower = LowerList,
    Upper = UpperList
  )

  return(ReturnDF)
}

INLARepPlot <- function(ModelList, ModelNames = NULL, Just = F,
                        DICOrder = F, CutOff = 0,
                        Family = "gaussian", Residual = T, CI = F,
                        Position = "stack", Plot = T){

  require(tidyverse); require(INLA); require(ggregplot)

  if(!class(ModelList)=="list"){
    ModelList <- list(ModelList)
  }

  OutputList <- lapply(ModelList, function(a) INLARep(a, Family = Family))

  for(i in 1:length(OutputList)){
    OutputList[[i]] <- OutputList[[i]] %>% mutate(
      Model = names(OutputList)[i],
      Var = rownames(OutputList[[i]])
    )
  }

  OutputList <- OutputList %>% bind_rows() %>%
    mutate(Var = factor(Var, levels = unique(Var)),
           Model = factor(Model, levels = unique(Model)))

  if(CI){

    for(i in unique(OutputList$Model)){

      Subdf <- OutputList %>% filter(Model == i) %>% slice(order(Var))
      N = Subdf %>% nrow
      Sums = c(1, 1-cumsum(Subdf$Mean))
      Sums = Sums[-length(Sums)]
      OutputList[OutputList$Model == i,c("Lower", "Upper")] <-
        OutputList %>% filter(Model == i) %>%
        mutate(Lower = Lower + Sums - Mean,
               Upper = Upper + Sums - Mean) %>% select(Lower, Upper)

    }

  }

  if(DICOrder){

    OutputList <- OutputList %>%
      mutate(Model = factor(OutputList$Model,
                            levels = levels(OutputList$Model)[(order(sapply(ModelList, MDIC), decreasing = T))]))

    OutputList$DIC <- sapply(ModelList, MDIC)[as.character(OutputList$Model)]
    OutputList$Competitive <- ifelse(OutputList$DIC<(min(OutputList$DIC)+CutOff), "*", " ")

  }

  OutputLong <- OutputList %>% rename(Variance = Mean)
  OutputLong[,c("Lower","Upper")] <- OutputLong %>%
    mutate(Lower = ifelse(Upper-Lower<0.01, NA, Lower),
           Upper = ifelse(Upper-Lower<0.01, NA, Upper)) %>% select(Lower, Upper)

  if(!Residual){
    OutputLong <- OutputLong %>% filter(!Var == "Residual")
  }

  if(Just){ Angle = 45; Hjust = 1 }else{ Angle = 0; Hjust = 0.5}

  RepPlot <- ggplot(OutputLong, aes(factor(Model), Variance, fill = Var)) +
    geom_col(position = Position) +
    labs(x = "Model") +
    theme(axis.text.x = element_text(angle = Angle, hjust = Hjust))

  if(DICOrder&CutOff>0){

    RepPlot <- RepPlot  + geom_text(aes(y = 0.95, label = Competitive))

  }

  if(CI){

    RepPlot <- RepPlot +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
      lims(y = c(0,max(OutputLong$Upper)))

  } else{
    RepPlot <- RepPlot +
      lims(y = c(0,1))

  }

  if(Plot){
    return(RepPlot)
  }else{
    return(OutputLong)
  }

}

