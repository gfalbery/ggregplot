# INLA ICC function

INLARep <- function(Model, Family = "gaussian",
                    Draw = F, NDraw = 1,
                    Return = "Raw",
                    SPDEComponent = "Variance",
                    Exclude = "Nowt",
                    SPDEModel = NULL,
                    ...){

  require(ggregplot); require(INLA)

  Marginals <- Model$marginals.hyperpar

  Marginals <- Marginals[which(!names(Marginals) %in% Exclude)]

  if(!Draw){

    SigmaList <- list()

    Parameters <- which(names(Marginals) %>% str_split(" ") %>%
                          map(1) %in%c("Precision","precision", "size"))

    if(length(Parameters)>0){

      HyperPars <- Marginals[Parameters]

      Sizes <- names(Marginals) %>% str_split(" ") %>% map(1) %in%c("size") %>%
        which()

      for(x in 1:(length(HyperPars))){

        tau <- HyperPars[[x]]

        if(length(Sizes)>0){

          if(x == Sizes){

            sigma <- inla.emarginal(function(x) x, tau)
            ci <- tau %>% inla.tmarginal(function(a) a, .) %>% inla.hpdmarginal(p = 0.95)

          }else{

            sigma <- inla.emarginal(function(x) 1/x, tau)
            ci <- tau %>% inla.tmarginal(function(a) 1/a, .) %>% inla.hpdmarginal(p = 0.95)

          }

        }else{

          sigma <- inla.emarginal(function(x) 1/x, tau)
          ci <- tau %>% inla.tmarginal(function(a) 1/a, .) %>% inla.hpdmarginal(p = 0.95)

        }

        SigmaList[[x]] <- sigma

      }

      NameList <- sapply(names(HyperPars), function(a) last(strsplit(a, " ")[[1]]))

      if(length(Sizes)>0){
        NameList[Sizes] <- "Residual"
      }

      substr(NameList, 1, 1) <- toupper(substr(NameList, 1, 1))

      if(any(NameList=="Observations")) NameList[NameList=="Observations"] <- "Residual"

      names(SigmaList) <- NameList

      if(any(names(Marginals) %>% str_detect("Range"))){

        Var <- names(Marginals)[which(names(Marginals) %>%
                                        str_detect("Range"))]

        Expl <- Var %>% str_split(" ") %>% last %>% last

        i1 = inla.spde.result(Model, Expl, SPDEModel)

        if(SPDEComponent == "Tau"){

          i2 = i1$marginals.tau[[1]] %>% inla.tmarginal(function(a) 1/a, .) %>% inla.mmarginal()

        }else if(SPDEComponent == "Variance"){

          i2 = i1$marginals.variance.nominal[[1]] %>% inla.mmarginal()

        }

        SigmaList$SPDE <- i2
        SigmaList$W <- NULL

      }

      if(Family == "binomial"){

        ReturnList <- sapply(SigmaList, function(a) (a)/c(sum(sapply(SigmaList, function(b) b))+pi^(2/3)))

      }

      if(Family == "gaussian"){

        ReturnList <- sapply(SigmaList, function(a) a/(sum(unlist(SigmaList))))

      }

      if(Family == "nbinomial"){

        Model %>% INLAFit(., ...) %>% na.omit %>% mean -> Beta0 #TestDF = Data, FixedCovar = FixedCovar, Locations = Locations, Mesh = Mesh) -> Beta0

        Ve <- sum(unlist(SigmaList))

        Expected <- exp(Beta0 + (0.5*(Ve))) #Expected values

        sapply(SigmaList, function(Va){

          (Expected*(exp(Va)-1))/(Expected*(exp(Ve)-1)+1)

        }) -> ReturnList

      }

      ReturnDF <- data.frame(
        Mean = ReturnList
      )

    }else{

      ReturnDF <- data.frame(Mean = 1)

    }

    return(ReturnDF)

  }else{ # Other end of if(!Draw) #####

    SigmaList <- list()

    Marginals %>% names %>%
      str_detect(c("Precision|precision|size")) %>%
      names(Marginals)[.] ->

      Parameters

    if(length(Parameters)>0){

      HyperPars <- Marginals[Parameters]

      Sizes <- names(Marginals) %>%
        str_split(" ") %>% map(1) %in%c("size") %>%
        which()

      for(x in 1:(length(HyperPars))){

        tau <- HyperPars[[x]]

        if(length(Sizes)>0){

          if(x == Sizes){

            tau %>% inla.rmarginal(NDraw, .) -> Sigma

          }else{

            tau %>% inla.tmarginal(function(a) 1/a, .) %>% inla.rmarginal(NDraw, .) -> Sigma

          }

        }else{

          tau %>% inla.tmarginal(function(a) 1/a, .) %>% inla.rmarginal(NDraw, .) -> Sigma

        }

        SigmaList[[x]] <- Sigma

      }

      NameList <- sapply(names(HyperPars), function(a) last(strsplit(a, " ")[[1]]))

      if(length(Sizes)>0){

        NameList[Sizes] <- "Residual"

      }

      substr(NameList, 1, 1) <- toupper(substr(NameList, 1, 1))

      if(any(NameList == "Observations")) NameList[NameList == "Observations"] <- "Residual"

      names(SigmaList) <- NameList

      if(any(Parameters %>% str_detect("Range"))){

        Var <- Parameters[which(names(Marginals) %>%
                                  str_detect("Range"))]

        Expl <- Var %>% str_split(" ") %>% last %>% last

        i1 = inla.spde.result(Model, Expl, SPDEModel)

        if(SPDEComponent == "Tau"){

          i2 = i1$marginals.tau[[1]] %>%
            inla.tmarginal(function(a) 1/a, .) %>%
            inla.rmarginal(NDraw, .)

        }

        if(SPDEComponent == "Variance"){

          i2 = i1$marginals.variance.nominal[[1]] %>%
            inla.rmarginal(NDraw, .)

        }

        SigmaList$SPDE <- i2

        SigmaList$W <- NULL

      }
    }

    SigmaList %>% bind_cols -> SigmaDF

    if(Family == "binomial"){

      Denominators <- rowSums(SigmaDF) + pi^(2/3)

      SigmaList %>% map_dfc(~.x/Denominators) -> ReturnDF

    }

    if(Family == "gaussian"){

      Denominators <- rowSums(SigmaDF)

      SigmaList %>% map_dfc(~.x/Denominators) -> ReturnDF

    }

    if(Family == "nbinomial"){

      Model %>% INLAFit(., Draw = T, NDraw = NDraw, ...) ->

        Beta01

      Beta01 %>% map_dbl(~.x %>% c %>% na.omit %>% mean) ->

        Beta0

      Ve <- SigmaDF %>% rowSums

      Expected <- exp(Beta0 + (0.5*(Ve))) #Expected values

      SigmaDF %>% apply(2, function(Va){

        (Expected*(exp(Va)-1))/(Expected*(exp(Ve)-1)+1)

      }) -> ReturnDF
    }

    if(Return == "Raw") return(ReturnDF) else{

      ReturnDF %>% as.data.frame %>%

        gather("Component") %>%

        na.omit %>%

        group_by(Component) %>%
        summarise(Mean = posterior.mode(as.mcmc(value)),
                  Lower = HPDinterval(as.mcmc(value))[1],
                  Upper = HPDinterval(as.mcmc(value))[2]) %>%

        as.data.frame() -> SummaryDF

      return(SummaryDF)

    }
  }
}

INLARepPlot <- function(ModelList,
                        ModelNames = NULL,
                        VarNames = NULL, VarOrder = NULL,
                        Just = F, Outline = F,
                        DICOrder = F, CutOff = 0,
                        Residual = T, CI = F,
                        Position = "stack", Plot = T,
                        ...){

  require(tidyverse); require(INLA);

  if(!class(ModelList)=="list"){

    ModelList <- list(ModelList)

  }

  if(is.null(ModelNames)){

    if(is.null(names(ModelList))){

      ModelNames <- 1:length(ModelList)

    }else{

      ModelNames <- names(ModelList)

    }
  }

  OutputList <- lapply(ModelList, function(a) INLARep(a, ...))

  names(OutputList) <- ModelNames

  for(i in 1:length(OutputList)){
    OutputList[[i]] <- OutputList[[i]] %>% mutate(
      Model = names(OutputList)[i],
      Var = rownames(OutputList[[i]])
    )
  }

  OutputList <- OutputList %>% bind_rows() %>%
    mutate(Var = factor(Var, levels = unique(Var)),
           Model = factor(Model, levels = unique(Model)))

  if(!is.null(VarNames)){
    levels(OutputList$Var) <- VarNames
  }

  if(!is.null(VarOrder)){
    OutputList$Var <- factor(OutputList$Var, levels = VarOrder)
  }

  if(CI){

    for(i in unique(OutputList$Model)){

      Subdf <- OutputList %>% filter(Model == i) %>% slice(order(Var))
      N = Subdf %>% nrow
      Sums = c(1, 1-cumsum(Subdf$Mean))
      Sums = Sums[-length(Sums)]
      OutputList[OutputList$Model == i,c("Lower", "Upper")] <-
        OutputList %>% filter(Model == i)

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

  if(!Residual){
    OutputLong <- OutputLong %>% filter(!Var == "Residual")
  }

  if(Just){ Angle = 45; Hjust = 1 }else{ Angle = 0; Hjust = 0.5}

  Colour <- ifelse(Outline, "black", NA)

  RepPlot <- ggplot(OutputLong, aes(factor(Model), Variance, fill = Var)) +
    geom_col(position = Position, colour = Colour) +
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
