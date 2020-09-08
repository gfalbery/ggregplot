INLAModelAdd <- function(Response, Explanatory, Add,
                         Random = NULL,
                         Rounds = Inf,
                         Clashes = NULL,
                         AllModels = F, BaseModel = F,
                         RandomModel = NULL, Family, Data, Delta = 2,
                         ReturnData = T,
                         AddSpatial = F, Coordinates = c("X", "Y"),
                         Groups = F, GroupVar = NULL){

  require(INLA); require(ggplot2)

  Data %<>% as.data.frame

  Explanatory2 <- paste(Explanatory, collapse = " + ")

  if(!is.null(Random)){

    Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")
    f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, " + ", Random2, collapse = " + ")))

  }else{

    f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, collapse = " + ")))

  }

  Base <- inla(f1,
               family = Family,
               data = Data,
               control.compute = list(dic = TRUE))

  ModelList <- AllModelList <- RemovedList <- FullFormulaList <- FormulaList <- list()
  DICList <- dDICList <- list()

  DICList[["Base"]] <- Base$dic$dic
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

          AllModelList[[length(AllModelList) + 1]] <- ModelList
          FullFormulaList[[length(FullFormulaList) + 1]] <- FormulaList

          DICList[[length(DICList) + 1]] <- sapply(ModelList, function(y) y$dic$dic)
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

  ReturnList <- list(FinalModel = FinalModel,
                     # AllModels = AllModelList,
                     Removed = RemovedList,
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

    SubCovar <- c(Explanatory, KeptCovar) %>% setdiff("1")

    print(SubCovar)

    Points <- Data %>% dplyr::select(all_of(Coordinates)) %>%
      as.data.frame

    Points %>%
      slice(sample(1:n(), 100)) %>% as.matrix %>%
      dist %>% c %>% max %>% divide_by(2) ->
      NullRangePrior; NullRangePrior

    Points %<>% as.matrix

    Mesh <- inla.mesh.2d(loc = Points,
                         max.edge = NullRangePrior/10,
                         cutoff = NullRangePrior/20)

    A3 <- inla.spde.make.A(Mesh, loc = Points) # Making A matrix

    spde <- inla.spde2.pcmatern(mesh = Mesh,
                                prior.range = c(NullRangePrior, 0.5),
                                prior.sigma = c(.5, .5)) # Making SPDE

    w.index <- inla.spde.make.index('w', n.spde = spde$n.spde)

    BaseLevels <- GetBaseLevels(SubCovar, Data)

    Xm <- model.matrix(as.formula(paste0("~ -1 +",
                                         paste(SubCovar, collapse = " + "))),
                       data = Data)

    X <- as.data.frame(Xm)[,!colnames(Xm)%in%BaseLevels]

    if(!is.null(Random)){

      Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")

      f1 <- as.formula(paste0("y", " ~ - 1 + Intercept + ",
                              paste(colnames(X), " + ", Random2, collapse = " + "),
                              "+ f(w, model = spde)"))

    }else{

      f1 <- as.formula(paste0("y", " ~ - 1 + Intercept + ",
                              paste(colnames(X), collapse = " + "),
                              "+ f(w, model = spde)"))

    }

    N <- nrow(Data)

    list(Intercept = rep(1, N), # Leave
         X = X) -> EffectsList

    if(!is.null(Random)){

      EffectsList[Random] <-
        map(Random, ~Data[,.x])

    }

    EffectsList$w <- w.index

    AList <- rep(1, length(EffectsList)-1) %>% as.list %>%
      append(list((A3)))

    SocialStack <- inla.stack(
      data = list(y = Data[,Response]),
      A = AList, # Vector of Multiplication factors
      effects = EffectsList) # Leave

    SpatialModel <- inla(f1, # f2 + SPDE random effect
                         family = Family,
                         data = inla.stack.data(SocialStack),
                         control.compute = list(dic = TRUE),
                         control.predictor = list(A = inla.stack.A(SocialStack))
    )

    SpatialList <- list(Model = SpatialModel,
                        Mesh = Mesh,
                        SPDE = spde)

    ReturnList$Spatial <- SpatialList

    if(Groups == T){

      print("Spatiotemporal!")

      Data$GroupVar <- Data[,GroupVar] %>% as.factor %>% as.numeric

      NGroup <- max(Data$GroupVar)

      TemporalA3 <- inla.spde.make.A(Mesh,
                                     loc = Points,
                                     repl = Data$GroupVar,
                                     n.repl = NGroup) # Making A matrix

      TemporalSPDE = inla.spde2.pcmatern(mesh = Mesh,
                                         prior.range = c(10, 0.5),
                                         prior.sigma = c(.5, .5)) # Making SPDE

      w.index.temporal <- inla.spde.make.index('wTemporal',
                                               n.spde = TemporalSPDE$n.spde,
                                               n.repl = NGroup)

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

      SpatiotemporalModel <- inla(fst, # Adding spatiotemporal effect
                                  family = Family,
                                  data = inla.stack.data(SocialStack),
                                  control.compute = list(dic = TRUE),
                                  control.predictor = list(A = inla.stack.A(SocialStack))
      )

      ReturnList$Spatial$SpatiotemporalModel <- SpatiotemporalModel

    }
  }

  if(ReturnData){

    ReturnList$Data <- Data

  }

  return(ReturnList)

}

