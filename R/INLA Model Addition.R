INLAModelAdd <- function(Response,
                         Data,
                         Explanatory = 1,
                         Add = NULL,
                         Random = NULL,
                         Rounds = Inf,
                         Clashes = NULL,
                         AllModels = F, BaseModel = F,
                         RandomModel = NULL,
                         Family = "gaussian",
                         NTrials = 1,
                         Delta = 2,
                         ReturnData = T,
                         Beep = F,
                         AddSpatial = F, Coordinates = c("X", "Y"),
                         Boundary = NULL,
                         Groups = F, GroupVar = NULL, GroupModel = "Rep"){

  require(INLA); require(ggplot2)

  Data %<>% as.data.frame %>%
    mutate_if(is.character, as.factor)

  Explanatory2 <- paste(Explanatory, collapse = " + ")

  if(!is.null(Random)){

    Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")
    f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, " + ", Random2, collapse = " + ")))

  }else{

    f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, collapse = " + ")))

  }

  Base <- inla(f1,
               family = Family, #Ntrials = NTrials,
               data = Data,
               control.compute = list(dic = TRUE))

  ModelList <- AllModelList <- RemovedList <- FullFormulaList <- FormulaList <- list()
  DICList <- dDICList <- list()

  DICList[["Base"]] <- Base$dic$dic
  FullFormulaList[["Base"]] <- f1

  AddList <- Add

  if(class(Add) == "list"){

    Add %<>% map_chr(~paste0(.x, collapse = " + "))

  }

  if(!is.null(Add)){

    for(x in 1:length(Add)){

      "Adding: " %>% paste0(Add[x]) %>% print

      Explanatory3 <- paste(c(Explanatory, Add[x]), collapse = " + ")

      if(!is.null(Random)){

        Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")
        f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, " + ", Random2, collapse = " + ")))

      }else{f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))}

      Model1 <- inla(f2,
                     family = Family, Ntrials = NTrials,
                     data = Data,
                     control.compute = list(dic = TRUE))

      ModelList[[Add[x]]] <- Model1

      FormulaList[[Add[x]]] <- f2

    }

    ModelList %>% MDIC %>% unlist -> DICValues

    names(DICValues) <- Add

    if(any(!is.finite(DICValues))){

      print("Warning: Null DIC Values")

      print(DICValues)

    }

    DICList[[2]] <- sapply(ModelList, function(y) y$dic$dic)
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

              if(!is.null(Random)){

                Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")
                f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, " + ", Random2, collapse = " + ")))

              }else{f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))}

              Model1 <- inla(f2,
                             family = Family, Ntrials = NTrials,
                             data = Data,
                             control.compute = list(dic = TRUE))

              ModelList[[Add2[x]]] <- Model1

              FormulaList[[Add2[x]]] <- f2

              #print(paste("Adding", Add2[x]))

            }

            AllModelList[[length(AllModelList) + 1]] <- ModelList
            FullFormulaList[[length(FullFormulaList) + 1]] <- FormulaList

            ModelList %>% MDIC %>% unlist -> DICValues

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

  if(AddSpatial){

    print("Adding Spatial!")

    FullSpatialList <- list()

    FixedCovar <- c(Explanatory, KeptCovar) %>% setdiff("1")# %>% intersect(colnames(Data))
    FixedCovar <- FixedCovar[!(FixedCovar %>% str_detect("f[(]"))]

    RandomAdd <- c(Explanatory, KeptCovar) %>% setdiff("1") %>% setdiff(colnames(Data))

    RandomAdd <- RandomAdd[RandomAdd %>% str_detect("f[(]")]

    if(length(RandomAdd)>0){

      RandomVar <- RandomAdd %>% str_split("f[(]") %>% map_chr(2) %>%
        str_split(", ") %>% map_chr(1) %>%
        str_trim()

    }else RandomAdd <- NULL

    if(class(Coordinates) == "list"){

      CoordinateNumber <- length(Coordinates)

      SubCoordinates <- Coordinates[[1]]

    }else{

      CoordinateNumber <- 1

      SubCoordinates <- Coordinates

    }

    for(i in 1:CoordinateNumber){

      print(paste0("Fitting Spatial Field ", i, "!"))

      if(i > 1){

        SubCoordinates <- Coordinates[[i]]

      }

      Points <- Data %>% dplyr::select(all_of(SubCoordinates)) %>%
        as.data.frame

      Points %>%
        slice(sample(1:n(), 100)) %>% as.matrix %>%
        dist %>% c %>% max %>% divide_by(2) ->
        NullRangePrior; NullRangePrior

      Points %<>% as.matrix

      Mesh <- inla.mesh.2d(loc = Points,
                           boundary = Boundary,
                           max.edge = NullRangePrior/10,
                           cutoff = NullRangePrior/20)

      A3 <- inla.spde.make.A(Mesh, loc = Points) # Making A matrix

      spde <- inla.spde2.pcmatern(mesh = Mesh,
                                  prior.range = c(NullRangePrior, 0.5),
                                  prior.sigma = c(.5, .5)) # Making SPDE

      w.index <- inla.spde.make.index('w', n.spde = spde$n.spde)

      BaseLevels <- GetBaseLevels(FixedCovar %>% intersect(names(Data)), Data)

      if(length(FixedCovar) > 0){

        Xm <- model.matrix(as.formula(paste0("~ -1 +",
                                             paste(FixedCovar, collapse = " + "))),
                           data = Data)

        X <- as.data.frame(Xm)[,!colnames(Xm)%in%BaseLevels]

      }else{

        X <- data.frame(Intercept = rep(1, nrow(Data)))

      }

      X %<>% rename_all(~str_replace_all(.x, ":", "_"))

      FixedCovar %<>% str_replace_all(":", "_")

      FormulaCovar <- colnames(X)

      if(!is.null(Random)){

        Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")

        FormulaCovar <- FormulaCovar %>% c(Random2)

      }

      if(!is.null(RandomAdd)){

        FormulaCovar <- FormulaCovar %>% c(RandomAdd)

      }

      if(length(FormulaCovar)>0){

        f1 <- as.formula(paste0("y", " ~ - 1 + Intercept + ",
                                paste(FormulaCovar, collapse = " + "),
                                "+ f(w, model = spde)"))

      }else{

        f1 <- as.formula(paste0("y", " ~ - 1 + Intercept + ",
                                "f(w, model = spde)"))

      }

      N <- nrow(Data)

      list(Intercept = rep(1, N), # Leave
           X = X) -> EffectsList

      if(!is.null(Random)){

        EffectsList[Random] <-
          map(Random, ~Data[,.x])

      }

      if(!is.null(RandomAdd)){

        EffectsList[RandomVar] <-
          map(RandomVar, ~Data[,.x])

      }

      EffectsList$w <- w.index

      AList <- rep(1, length(EffectsList)-1) %>% as.list %>%
        append(list((A3)))

      SocialStack <- inla.stack(
        data = list(y = Data[,Response]),
        A = AList, # Vector of Multiplication factors
        effects = EffectsList) # Leave

      SpatialModel <- inla(f1, # f2 + SPDE random effect
                           family = Family, Ntrials = NTrials,
                           data = inla.stack.data(SocialStack),
                           control.compute = list(dic = TRUE),
                           control.predictor = list(A = inla.stack.A(SocialStack))
      )

      SpatialList <- list(Model = SpatialModel,
                          Mesh = Mesh,
                          SPDE = spde)

      FullSpatialList <- SpatialList

      if(Groups == T){

        print("Spatiotemporal!")

        Data$GroupVar <- Data[,GroupVar] %>% as.factor %>% as.numeric

        NGroup <- max(Data$GroupVar)

        TemporalSPDE = inla.spde2.pcmatern(mesh = Mesh,
                                           prior.range = c(10, 0.5),
                                           prior.sigma = c(.5, .5)) # Making SPDE

        if(GroupModel == "Rep"){

          w.index.temporal <- inla.spde.make.index('wTemporal',
                                                   n.spde = TemporalSPDE$n.spde,
                                                   n.repl = NGroup)

          TemporalA3 <- inla.spde.make.A(Mesh,
                                         loc = Points,
                                         repl = Data$GroupVar,
                                         n.repl = NGroup) # Making A matrix

        }else{

          w.index.temporal <- inla.spde.make.index('wTemporal',
                                                   n.spde = TemporalSPDE$n.spde,
                                                   n.group = NGroup)

          TemporalA3 <- inla.spde.make.A(Mesh,
                                         loc = Points,
                                         group = Data$GroupVar,
                                         n.group = NGroup) # Making A matrix

        }

        list(Intercept = rep(1, N), # Leave
             X = X) -> EffectsList

        if(!is.null(Random)){

          EffectsList[Random] <-
            map(Random, ~Data[,.x])

        }

        EffectsList$w <- w.index.temporal

        AList <- rep(1, length(EffectsList)-1) %>% as.list %>%
          append(list(TemporalA3))

        SocialStack <- inla.stack(
          data = list(y = Data[,Response]),
          A = AList, # Vector of Multiplication factors
          effects = EffectsList) # Leave

        if(GroupModel == "Rep"){

          if(!is.null(Random)){

            Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")

            fst <- as.formula(paste0("y", " ~ - 1 + Intercept + ",
                                     paste(colnames(X), " + ", Random2, collapse = " + "),
                                     "+ f(wTemporal, model = TemporalSPDE, replicate = wTemporal.repl)"))

          }else{

            fst <- as.formula(paste0("y", " ~ - 1 + Intercept + ",
                                     paste(colnames(X), collapse = " + "),
                                     "+ f(wTemporal, model = TemporalSPDE, replicate = wTemporal.repl)"))

          }

        }else{

          if(!is.null(Random)){

            Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")

            fst <- as.formula(paste0("y", " ~ - 1 + Intercept + ",
                                     paste(colnames(X), " + ", Random2, collapse = " + "),
                                     " + f(wTemporal, model = TemporalSPDE, group = wTemporal.group, control.group = list(model = '",GroupModel,"'))"))

          }else{

            fst <- as.formula(paste0("y", " ~ - 1 + Intercept + ",
                                     paste(colnames(X), collapse = " + "),
                                     " + f(wTemporal, model = TemporalSPDE, group = wTemporal.group, control.group = list(model = '",GroupModel,"'))"))

          }
        }

        SpatiotemporalModel <- inla(fst, # Adding spatiotemporal effect
                                    family = Family, Ntrials = NTrials,
                                    data = inla.stack.data(SocialStack),
                                    control.compute = list(dic = TRUE),
                                    control.predictor = list(A = inla.stack.A(SocialStack))
        )

        SpatialList$SpatiotemporalModel <- SpatiotemporalModel

        if(CoordinateNumber>1){

          FullSpatialList[[i]] <- SpatialList

        }else{

          FullSpatialList <- SpatialList

        }
      }
    }

    ReturnList$Spatial <- FullSpatialList

  }

  if(ReturnData){

    ReturnList$Data <- Data

  }

  if(Beep) beepr::beep()

  return(ReturnList)

}

