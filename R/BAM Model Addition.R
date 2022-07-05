BAMModelAdd <- function(Response,
                        Data,
                        Explanatory = 1, ScaleVariables = T,
                        Add = NULL,
                        Rounds = Inf,
                        Clashes = NULL,
                        AllModels = F, BaseModel = F,
                        Family = gaussian(),
                        Delta = 2,
                        ReturnData = T,
                        PP = NULL,
                        Select = T,
                        Beep = F){

  require(mgcv); require(ggplot2)

  print(paste0("Response: ", Response))
  print(paste0("Explanatory: ", paste0(Explanatory, collapse = ", ")))
  if(ScaleVariables) print("Scaling variables to SD and mean")

  Data %<>%
    as.data.frame %>%
    mutate_if(is.character, as.factor)

  if(ScaleVariables){

    Classes <- Data %>%
      dplyr::select(intersect(Explanatory, colnames(Data)),
                    intersect(Add, colnames(Data))) %>%
      sapply(class)

    ToScale <- names(Classes[Classes %in% c("integer", "numeric")])

    Data[,paste0(ToScale, ".Original")] <- Data[,ToScale]

    Data %<>% mutate_at(ToScale, ~c(scale(.x)))

    if(Family$family == "gaussian"){

      Data[,paste0(Response, ".Original")] <- Data[,Response]

      Data %<>% mutate_at(Response, ~c(scale(.x)))

    }
  }

  Explanatory2 <- c(paste(Explanatory, collapse = " + "))

  f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, collapse = " + ")))

  print(paste0("Running Base Model: ", paste0(Response, " ~ ", paste(Explanatory2, collapse = " + "))))

  Base <- bam(f1,
              family = Family,
              paraPen = PP,
              select = Select,
              data = Data
  )

  ModelList <- AllModelList <- RemovedList <- FullFormulaList <- FormulaList <- list()
  DICList <- dDICList <- list()

  DICList[["Base"]] <- Base %>% deviance
  FullFormulaList[["Base"]] <- f1

  AddList <- Add

  if(class(Add) == "list"){

    Add %<>% map_chr(~paste0(.x, collapse = " + "))

  }

  if(!is.null(Add)){

    for(x in 1:length(Add)){

      "Adding: " %>% paste0(Add[x]) %>% print

      Explanatory3 <- paste(c(Explanatory, Add[x]), collapse = " + ")

      f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))

      Model1 <- bam(f2,
                    family = Family,
                    data = Data,
                    paraPen = PP,
                    select = Select)

      ModelList[[Add[x]]] <- Model1

      FormulaList[[Add[x]]] <- f2

    }

    ModelList %>% sapply(deviance) %>% unlist -> DICValues

    names(DICValues) <- Add

    if(any(!is.finite(DICValues))){

      print("Warning: Null DIC Values")

      print(DICValues)

    }

    DICList[[2]] <- sapply(ModelList, function(y) y %>% deviance)
    names(DICList[[2]]) <- Add
    dDICList[[1]] <- DICList[[2]] - DICList[[1]]
    names(dDICList[[1]]) <- Add
    RemovedList[[1]] <- Add

    FullFormulaList[[2]] <- FormulaList

    AllModelList[[1]] <- Base
    AllModelList[[2]] <- ModelList

    NewExplanatory <- Explanatory

    Add2 <- Add

    KeptCovar <- c()

    if((min(dDICList[[length(dDICList)]]) < -Delta)&(Rounds>1)&(length(Add2)>0)){

      while((min(dDICList[[length(dDICList)]]) < -Delta)&(Rounds>1)&(length(Add2)>0)){

        Rounds <- Rounds - 1

        Kept <- Add2[which(dDICList[[length(dDICList)]] == min(dDICList[[length(dDICList)]]))]

        KeptCovar <- c(KeptCovar, Kept)

        ModelList <- FormulaList <- list()

        NewExplanatory <- c(NewExplanatory, Add2[which(dDICList[[length(dDICList)]] == min(dDICList[[length(dDICList)]]))])

        Add2 <- Add2[-which(dDICList[[length(dDICList)]] == min(dDICList[[length(dDICList)]]))]

        if(length(Add2)>0){

          print(paste("Keeping", Kept))

          print(Text <- paste("Run", length(DICList)))

          KeptSplit <- Kept %>% str_split(" [+] ") %>% unlist %>% as.character

          if(any(KeptSplit %in% unlist(Clashes))){

            ClashRemove <-
              Clashes[Clashes %>% map_lgl(~any(KeptSplit %in% .x))] %>%
              unlist %>% unique

            Add2 <-
              AddList[!AddList %>%
                        map_lgl(~any(ClashRemove %in% .x))] %>%
              map_chr(~paste0(.x, collapse = " + ")) %>%
              intersect(Add2)

            "Removing clashes: " %>% paste0(paste0(ClashRemove, collapse = "; ")) %>% print

          }

          if(length(Add2)>0){

            for(x in 1:length(Add2)){

              "Adding: " %>% paste0(Add2[x]) %>% print

              Explanatory3 <- paste(c(NewExplanatory, Add2[x]), collapse = " + ")

              f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))

              Model1 <- bam(f2,
                            family = Family,
                            data = Data,
                            paraPen = PP,
                            select = Select)

              ModelList[[Add2[x]]] <- Model1

              FormulaList[[Add2[x]]] <- f2

              #print(paste("Adding", Add2[x]))

            }

            AllModelList[[length(AllModelList) + 1]] <- ModelList
            FullFormulaList[[length(FullFormulaList) + 1]] <- FormulaList

            ModelList %>% sapply(deviance) %>% unlist -> DICValues

            names(DICValues) <- Add2

            if(any(!is.finite(DICValues))){

              print("Warning: Null DIC Values")

              print(DICValues)

            }

            DICList[[length(DICList) + 1]] <- DICValues
            names(DICList[[length(DICList)]]) <- Add2
            dDICList[[length(dDICList) + 1]] <- DICList[[length(DICList)]] - min(DICList[[length(DICList)-1]])
            names(dDICList[[length(dDICList)]]) <- Add2

            RemovedList[[length(RemovedList) + 1]] <- Add2

          }else{

            AllModelList[[length(AllModelList) + 1]] <-
              AllModelList[[length(AllModelList)]][Kept]

            FullFormulaList[[length(FullFormulaList) + 1]] <-
              FullFormulaList[[length(FullFormulaList)]][Kept]

            DICList[[length(DICList) + 1]] <-
              DICList[[length(DICList)]][Kept]

            names(DICList[[length(DICList)]]) <- Kept

            dDICList[[length(dDICList) + 1]] <-
              min(dDICList[[length(dDICList)]])

            names(dDICList[[length(dDICList)]]) <- Kept

            RemovedList[[length(RemovedList) + 1]] <- Add2

          }
        }
      }

      if(length(dDICList)>0){

        if(all(last(dDICList)< -Delta)){

          FinalModel <- AllModelList[[length(AllModelList)]][[1]]
          FinalFormula <- FullFormulaList[[length(AllModelList)]][[1]]

          print(paste("Keeping Everything!"))

        }else{

          FinalModel <- AllModelList[[length(AllModelList)-1]][[which(dDICList[[length(dDICList)-1]] == min(dDICList[[length(dDICList)-1]]))]]
          FinalFormula <- FullFormulaList[[length(AllModelList)-1]][[which(dDICList[[length(dDICList)-1]] == min(dDICList[[length(dDICList)-1]]))]]

          print(paste("Not Keeping ", paste(Add2, collapse = " ")))

        }

      }else{

        FinalModel <- Base
        FinalFormula <- f1

        print("Nothing Kept")

      }

    }else{

      FinalModel <- Base
      FinalFormula <- f1

      print("Nothing Kept")

    }

  }else{

    FinalModel <- Base
    FinalFormula <- f1

    KeptCovar <- c()

  }

  ReturnList <- list(FinalModel = FinalModel,
                     # AllModels = AllModelList,
                     Removed = RemovedList,
                     Explanatory = Explanatory,
                     Kept = KeptCovar,
                     DIC = DICList,
                     dDIC = dDICList,
                     FormulaList = FullFormulaList,
                     FinalFormula = FinalFormula)

  if(AllModels){

    ReturnList$AllModels <- AllModelList

  }

  if(BaseModel){

    ReturnList$Base <- Base

  }

  if(ReturnData){

    ReturnList$Data <- Data

  }

  if(Beep) beepr::beep()

  return(ReturnList)

}

