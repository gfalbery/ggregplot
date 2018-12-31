

INLAModelSel <- function(Response, Explanatory, Random, RandomModel, Family, Data, Delta = 2){

  require(INLA); require(ggplot2)

  Explanatory2 <- paste(Explanatory, collapse = " + ")

  Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")

  f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, " + ", Random2, collapse = " + ")))

  FullModel <-   inla(f1,
                      family = Family,
                      data = Data,
                      control.compute = list(dic = TRUE))

  ModelList <- AllModelList <- RemovedList <- FullFormulaList <- FormulaList <- list()
  DICList <- dDICList <- list()

  DICList[[1]] <- FullModel$dic$dic
  FullFormulaList[[1]] <- f1


  for(x in 1:length(Explanatory)){

    Explanatory3 <- paste(Explanatory[-x], collapse = " + ")
    f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, " + ", Random2, collapse = " + ")))

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
  names(dDICList[[1]]) <- Explanatory
  RemovedList[[1]] <- Explanatory

  FullFormulaList[[2]] <- FormulaList

  AllModelList[[1]] <- FullModel
  AllModelList[[2]] <- ModelList

  NewExplanatory <- Explanatory

  while(min(dDICList[[length(dDICList)]]) < Delta&length(NewExplanatory)>0){
    
    print(paste("Losing", names(dDICList[[length(dDICList)]])[which(dDICList[[length(dDICList)]] == min(dDICList[[length(dDICList)]]))]))
    print(Text <- paste("Run", length(DICList)))

    ModelList <- FormulaList <- list()

    NewExplanatory <- NewExplanatory[-which(dDICList[[length(dDICList)]]==min(dDICList[[length(dDICList)]]))]

    if(length(NewExplanatory) > 0){
      for(x in 1:length(NewExplanatory)){

        Explanatory3 <- paste(NewExplanatory[-x], collapse = " + ")

        f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, " + ", Random2, collapse = " + ")))

        Model1 <- inla(f2,
                       family = Family,
                       data = Data,
                       control.compute = list(dic = TRUE))

        ModelList[[x]] <- Model1

        FormulaList[[x]] <- f2

      }

    }else{

      f2 <- as.formula(paste0(Response, " ~ ", paste("1 + ", Random2, collapse = " + ")))

      Model1 <- inla(f2,
                     family = Family,
                     data = Data,
                     control.compute = list(dic = TRUE))

      ModelList[[x]] <- Model1

      FormulaList[[x]] <- f2

    }

    AllModelList[[length(AllModelList)+1]] <- ModelList
    FullFormulaList[[length(FullFormulaList)+1]] <- FormulaList

    DICList[[length(DICList)+1]] <- sapply(ModelList, function(y) y$dic$dic)
    names(DICList[[length(DICList)]]) <- NewExplanatory
    dDICList[[length(dDICList)+1]] <- DICList[[length(DICList)]] - min(DICList[[length(DICList)-1]])
    names(dDICList[[length(dDICList)]]) <- NewExplanatory

    RemovedList[[length(RemovedList)+1]] <- NewExplanatory

  }

  FinalModel <- AllModelList[[length(AllModelList)-1]][[which(dDICList[[length(dDICList)-1]]==min(dDICList[[length(dDICList)-1]]))]]
  FinalFormula <- FullFormulaList[[length(AllModelList)-1]][[which(dDICList[[length(dDICList)-1]]==min(dDICList[[length(dDICList)-1]]))]]

  return(list(FinalModel = FinalModel,
              AllModels = AllModelList,
              Removed = RemovedList,
              DIC = DICList,
              dDIC = dDICList,
              FormulaList = FullFormulaList,
              FinalFormula = FinalFormula))
}

