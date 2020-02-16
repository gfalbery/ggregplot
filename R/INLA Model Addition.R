INLAModelAdd <- function(Response, Explanatory, Add, Random = NULL,
                         Rounds = Inf,
                         Clashes = NULL,
                         AllModels = F,
                         RandomModel = NULL, Family, Data, Delta = 2){

  require(INLA); require(ggplot2)

  Explanatory2 <- paste(Explanatory, collapse = " + ")

  if(!is.null(Random)){

    Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")
    f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, " + ", Random2, collapse = " + ")))

  }else{f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, collapse = " + ")))}

  BaseModel <-   inla(f1,
                      family = Family,
                      data = Data,
                      control.compute = list(dic = TRUE))

  ModelList <- AllModelList <- RemovedList <- FullFormulaList <- FormulaList <- list()
  DICList <- dDICList <- list()

  DICList[["Base"]] <- BaseModel$dic$dic
  FullFormulaList[["Base"]] <- f1

  for(x in 1:length(Add)){

    "Adding: " %>% paste0(Add[x]) %>% print

    Explanatory3 <- paste(c(Explanatory, Add[x]), collapse = " + ")

    if(!is.null(Random)){

      Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")
      f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, " + ", Random2, collapse = " + ")))

    }else{f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))}

    Model1 <- inla(f2,
                   family = Family,
                   data = Data,
                   control.compute = list(dic = TRUE))

    ModelList[[Add[x]]] <- Model1

    FormulaList[[Add[x]]] <- f2

  }

  DICList[[2]] <- sapply(ModelList, function(y) y$dic$dic)
  names(DICList[[2]]) <- Add
  dDICList[[1]] <- DICList[[2]] - DICList[[1]]
  names(dDICList[[1]]) <- Add
  RemovedList[[1]] <- Add

  FullFormulaList[[2]] <- FormulaList

  AllModelList[[1]] <- BaseModel
  AllModelList[[2]] <- ModelList

  NewExplanatory <- Explanatory

  Add2 <- Add

  if((min(dDICList[[length(dDICList)]]) < -Delta)&(Rounds>1)&(length(Add2)>0)){

    while((min(dDICList[[length(dDICList)]]) < -Delta)&(Rounds>1)&(length(Add2)>0)){

      Rounds <- Rounds - 1

      Kept <- Add2[which(dDICList[[length(dDICList)]] == min(dDICList[[length(dDICList)]]))]

      print(paste("Keeping", Kept))

      print(Text <- paste("Run", length(DICList)))

      ModelList <- FormulaList <- list()

      NewExplanatory <- c(NewExplanatory, Add2[which(dDICList[[length(dDICList)]] == min(dDICList[[length(dDICList)]]))])
      Add2 <- Add2[-which(dDICList[[length(dDICList)]] == min(dDICList[[length(dDICList)]]))]

      if(Kept %in% unlist(Clashes)){

        ClashRemove <- Clashes[Clashes %>% map_lgl(~Kept %in% .x)] %>% unlist

        Add2 <- Add2 %>% setdiff(ClashRemove)

        "Removing clashes: " %>% paste0(paste0(ClashRemove, collapse = "; ")) %>% print

      }

      if(length(Add2)>0){

        for(x in 1:length(Add2)){

          "Adding: " %>% paste0(Add2[x]) %>% print

          Explanatory3 <- paste(c(NewExplanatory, Add2[x]), collapse = " + ")

          if(!is.null(Random)){

            Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")
            f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, " + ", Random2, collapse = " + ")))

          }else{f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))}

          Model1 <- inla(f2,
                         family = Family,
                         data = Data,
                         control.compute = list(dic = TRUE))

          ModelList[[Add2[x]]] <- Model1

          FormulaList[[Add2[x]]] <- f2

          #print(paste("Adding", Add2[x]))

        }

        AllModelList[[length(AllModelList)+1]] <- ModelList
        FullFormulaList[[length(FullFormulaList)+1]] <- FormulaList

        DICList[[length(DICList)+1]] <- sapply(ModelList, function(y) y$dic$dic)
        names(DICList[[length(DICList)]]) <- Add2
        dDICList[[length(dDICList)+1]] <- DICList[[length(DICList)]] - min(DICList[[length(DICList)-1]])
        names(dDICList[[length(dDICList)]]) <- Add2

        RemovedList[[length(RemovedList)+1]] <- Add2

      }
    }

    if(length(dDICList)>0){

      FinalModel <- AllModelList[[length(AllModelList)-1]][[which(dDICList[[length(dDICList)-1]] == min(dDICList[[length(dDICList)-1]]))]]
      FinalFormula <- FullFormulaList[[length(AllModelList)-1]][[which(dDICList[[length(dDICList)-1]] == min(dDICList[[length(dDICList)-1]]))]]

      print(paste("Not Keeping ", paste(Add2, collapse = " ")))

    }else{

      FinalModel <- BaseModel
      FinalFormula <- f1

      print("Nothing Kept")

    }

  }else{

    FinalModel <- BaseModel
    FinalFormula <- f1

    print("Nothing Kept")

  }

  ReturnList <- list(FinalModel = FinalModel,
                     # AllModels = AllModelList,
                     Removed = RemovedList,
                     DIC = DICList,
                     dDIC = dDICList,
                     FormulaList = FullFormulaList,
                     FinalFormula = FinalFormula)

  if(AllModels = T){

    ReturnList$AllModels <- AllModelList

  }

  return(ReturnList)

}

