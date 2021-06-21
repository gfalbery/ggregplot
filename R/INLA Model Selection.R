
INLAModelSel <- function(Response,
                         Explanatory,
                         Random = NULL, RandomModel = NULL,
                         Family, Data, Delta = 2,
                         ScaleVariables = T,

                         AllModels = F, FullModelReturn = F,
                         ReturnData = T,
                         Beep = T,

                         ...){

  require(INLA); require(ggplot2); require(magrittr)

  Data %<>% as.data.frame %>%
    mutate_if(is.character, as.factor)

  if(ScaleVariables){

    Classes <- Data %>%
      dplyr::select(intersect(Explanatory, colnames(Data))) %>%
      sapply(class)

    ToScale <- names(Classes[Classes %in% c("integer", "numeric")])

    Data[,paste0(ToScale, ".Original")] <- Data[,ToScale]

    Data %<>% mutate_at(ToScale, ~c(scale(.x)))

    if(Family == "gaussian"){

      Data[,paste0(Response, ".Original")] <- Data[,Response]

      Data %<>% mutate_at(Response, ~c(scale(.x)))

    }
  }

  Explanatory2 <- paste(Explanatory, collapse = " + ")

  if(!is.null(Random)){

    Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")

    f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, " + ", Random2, collapse = " + ")))

  }else{

    f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, collapse = " + ")))

  }

  FullModel <- inla(f1,
                    family = Family,
                    data = Data,
                    control.compute = list(dic = TRUE))

  ModelList <- AllModelList <- RemovedList <- FullFormulaList <- FormulaList <- list()
  DICList <- dDICList <- list()

  DICList[[1]] <- FullModel$dic$dic
  FullFormulaList[[1]] <- f1

  for(x in 1:length(Explanatory)){

    Explanatory3 <- paste(Explanatory[-x], collapse = " + ")

    if(!is.null(Random)){

      f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, " + ", Random2, collapse = " + ")))

    } else{

      f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))

    }

    Model1 <- inla(f2,
                   family = Family,
                   data = Data,
                   control.compute = list(dic = TRUE))

    ModelList[[x]] <- Model1

    FormulaList[[x]] <- f2

  }

  DICList[[2]] <- sapply(ModelList, function(y) y$dic$dic)

  names(DICList[[2]]) <- Explanatory

  dDICList[[1]] <- DICList[[2]] - DICList[[1]]

  names(dDICList[[1]]) <-
    RemovedList[[1]] <-
    Explanatory

  FullFormulaList[[2]] <- FormulaList

  AllModelList[[1]] <- FullModel
  AllModelList[[2]] <- ModelList

  NewExplanatory <- Explanatory

  while((min(last(dDICList)) < Delta) & (length(NewExplanatory)>0)){

    print(paste("Losing", names(last(dDICList))[which(last(dDICList) == min(last(dDICList)))]))

    print(Text <- paste("Run", length(DICList)))

    ModelList <- FormulaList <- list()

    NewExplanatory <- NewExplanatory[-which(last(dDICList)==min(last(dDICList)))]

    if(length(NewExplanatory) > 0){

      for(x in 1:length(NewExplanatory)){

        Explanatory3 <- paste(NewExplanatory[-x], collapse = " + ")

        if(!is.null(Random)){

          f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, " + ", Random2, collapse = " + ")))

        } else{

          f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))

        }

        Model1 <- inla(f2,
                       family = Family,
                       data = Data,
                       control.compute = list(dic = TRUE))

        ModelList[[x]] <- Model1

        FormulaList[[x]] <- f2

      }

    }else{

      if(!is.null(Random)){

        f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, " + ", Random2, collapse = " + ")))

      } else{

        f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))

      }

      Model1 <- inla(f2,
                     family = Family,
                     data = Data,
                     control.compute = list(dic = TRUE))

      ModelList[[x]] <- Model1

      FormulaList[[x]] <- f2

    }

    AllModelList[[length(AllModelList) + 1]] <- ModelList
    FullFormulaList[[length(FullFormulaList) + 1]] <- FormulaList

    DICList[[length(DICList) + 1]] <- sapply(ModelList, function(y) y$dic$dic)

    names(DICList[[length(DICList)]]) <- NewExplanatory

    dDICList[[length(dDICList) + 1]] <- DICList[[length(DICList)]] - min(DICList[[length(DICList) - 1]])

    names(dDICList[[length(dDICList)]]) <-
      RemovedList[[length(RemovedList) + 1]] <-
      NewExplanatory

  }

  FinalModel <- AllModelList[[length(AllModelList) - 1]][[which(dDICList[[length(dDICList) - 1]]==min(dDICList[[length(dDICList) - 1]]))]]

  FinalFormula <- FullFormulaList[[length(AllModelList) - 1]][[which(dDICList[[length(dDICList) - 1]]==min(dDICList[[length(dDICList) - 1]]))]]

  ReturnList <- list(FinalModel = FinalModel,
                     AllModels = AllModelList,
                     Removed = RemovedList,
                     DIC = DICList,
                     dDIC = dDICList,
                     FormulaList = FullFormulaList,
                     FinalFormula = FinalFormula)


  if(AllModels){

    ReturnList$AllModels <- AllModelList

  }

  if(FullModelReturn){

    ReturnList$FullModel <- FullModel

  }

  if(ReturnData){

    ReturnList$Data <- Data

  }

  if(Beep) beepr::beep()

  return(ReturnList)

}

