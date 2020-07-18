
# INLA Predictions ####

INLAFit <- function(Model, TestDF,
                    FixedCovar = NULL,
                    HoldFixed = NULL,
                    HoldRandom = NULL,
                    Locations = NULL,
                    Mesh = NULL, SPDEModel = NULL,
                    Groups = 1, GroupVariable = NULL,
                    Draw = F, NDraw = 1,
                    Return = "Vector"){

  if(!is.null(HoldFixed)){

    TestDF %>% dplyr::select(vars(is.numeric)) %>%
      names %>% intersect(HoldFixed) ->

      HoldFixedNumeric

    TestDf %>% mutate_at(HoldFixedNumeric, mean) ->

      TestDF

    HoldFixedNumeric %>% setdiff(HoldFixed, .) -> HoldFixedCategorical

  }

  if(!Draw){

    require(INLA)

    Ranges <-

      rownames(Model$summary.hyperpar)[str_detect(rownames(Model$summary.hyperpar), "Range")] %>%
      str_split(" ") %>% map_chr(last)

    RandomEstimates <- Model$summary.random

    RandomCovar <- names(RandomEstimates) %>% setdiff(Ranges)

    if(length(RandomCovar)>0){

      lapply(RandomCovar, function(a){

        TestDF[,a] <- as.factor(TestDF[,a])

        model.matrix(as.formula(paste0("~ -1 + ", a)), data = TestDF) -> NewDF

        colnames(NewDF) %>% str_replace_all(a, paste0(a, ".")) ->
          colnames(NewDF)

        NewDF <- NewDF %>% as.data.frame()

        return(NewDF)

      }) %>% bind_cols() %>% as.matrix -> RandomXMatrix

      lapply(1:length(RandomCovar), function(i){

        RandomEstimates[[RandomCovar[i]]]$mean ->

          FocalEstimates

        if(RandomCovar[i] %in% HoldRandom){

          FocalEstimates[] <- mean(FocalEstimates)

        }

        names(FocalEstimates) <- paste0(RandomCovar[i], ".", RandomEstimates[[RandomCovar[i]]]$ID)

        return(FocalEstimates)

      }) %>% unlist -> RandomEstimateVector

    }

    # Fixed effects ####

    FixedEstimates <- Model$summary.fixed

    TestDF$Intercept <- 1

    if(!is.null(FixedCovar)){

      f1 <- as.formula(paste0("~ 1 +", paste0(FixedCovar, collapse = " + ")))

      FixedXMatrix <- model.matrix(f1, data = TestDF) %>% as.matrix

      colnames(FixedXMatrix)[1] <- "Intercept"

    }else{

      f1 <- as.formula("~ 1")

      FixedXMatrix <- model.matrix(f1, data = TestDF)# %>% as.matrix

      colnames(FixedXMatrix)[1] <- "Intercept"

    }

    FixedCovar2 <- rownames(FixedEstimates)

    FixedEstimates$mean %>% unlist -> FixedEstimateVector

    names(FixedEstimateVector) <- FixedCovar2 %>% str_replace_all("[(]Intercept[)]", "Intercept")

    # Putting them together

    if(length(RandomCovar)>0){

      XMatrix <- cbind(FixedXMatrix, RandomXMatrix)

      SharedNames <-
        intersect(names(FixedEstimateVector), names(FixedEstimateVector)) %>%
        intersect(colnames(XMatrix))

      EstimateVector <- c(FixedEstimateVector, RandomEstimateVector)[colnames(XMatrix)]

      if(Return == "Vector"){

        (EstimateVector[colnames(XMatrix)] %*% t(XMatrix)) %>% c -> Predictions

      }else{

        (EstimateVector[colnames(XMatrix)] * t(XMatrix)) %>% t -> Predictions

      }

    }else{

      XMatrix <- FixedXMatrix

      SharedNames <- intersect(names(FixedEstimateVector), colnames(XMatrix))

      EstimateVector <- c(FixedEstimateVector)[SharedNames]

      if(Return == "Vector"){

        (EstimateVector[SharedNames] %*% t(XMatrix[,SharedNames])) %>% c ->

          Predictions

      }else{

        (EstimateVector[SharedNames] * t(XMatrix[,SharedNames])) %>% t ->

          Predictions

      }
    }

    if(length(Ranges)>0){

      if(!class(Locations) == "matrix"){

        Locations <- as.matrix(Locations)

      }

      Projection <-
        inla.mesh.projector(mesh = Mesh, loc = Locations, dims = c(300, 300))

      Model$summary.random[[Ranges]]$mean %>%
        split(1:Groups) %>%
        map(~inla.mesh.project(Projection, .x)) %>%

        bind_cols ->

        WPredictions

      names(WPredictions) <- paste0(Ranges, ".", names(WPredictions))

      if(!is.null(GroupVariable)){

        TestDF$GroupVariable <- TestDF[,GroupVariable]

        TestDF %>%
          mutate(Value = 1) %>%
          #select(GroupVariable, Value) %>%
          pivot_wider(names_from = "GroupVariable",
                      values_from = "Value") ->

          GroupVarMatrix

        GroupVarMatrix[,gtools::mixedsort(as.character(1:Groups))] ->

          GroupVarMatrix

        GroupVarMatrix[is.na(GroupVarMatrix)] <- 0

        WPredictions*GroupVarMatrix ->

          WPredictions

      }

      if(Return == "Vector"){

        FullPredictions <- Predictions + rowSums(WPredictions)

      }else{

        FullPredictions <- Predictions %>%
          as_tibble %>%
          bind_cols(WPredictions) %>%
          as.data.frame()

      }

    }else{

      FullPredictions <- Predictions

    }

    FullPredictions %>% return

  }else{

    Ranges <-

      rownames(Model$summary.hyperpar)[str_detect(rownames(Model$summary.hyperpar), "Range")] %>%
      str_split(" ") %>% map_chr(last)

    RandomEstimates <- Model$summary.random

    RandomCovar <- names(RandomEstimates) %>% setdiff(Ranges)

    if(length(RandomCovar)>0){

      lapply(RandomCovar, function(a){

        TestDF[,a] <- as.factor(TestDF[,a])

        model.matrix(as.formula(paste0("~ -1 + ", a)), data = TestDF) -> NewDF

        colnames(NewDF) %>% str_replace_all(a, paste0(a, ".")) ->
          colnames(NewDF)

        NewDF <- NewDF %>% as.data.frame()

        return(NewDF)

      }) %>% bind_cols() %>% as.matrix -> RandomXMatrix

      1:length(RandomCovar) %>% map_dfc(function(i){

        Model$marginals.random[[RandomCovar[i]]] %>% map(~inla.rmarginal(NDraw, .x)) ->

          FocalEstimates

        names(FocalEstimates) <- paste0(RandomCovar[i], ".", RandomEstimates[[RandomCovar[i]]]$ID)

        return(FocalEstimates)

      }) %>% as.data.frame() -> RandomEstimateDF

    }

    # Fixed effects ####

    FixedEstimates <- Model$summary.fixed

    TestDF$Intercept <- 1

    if(!is.null(FixedCovar)){

      f1 <- as.formula(paste0("~ 1 +", paste0(FixedCovar, collapse = " + ")))

      FixedXMatrix <- model.matrix(f1, data = TestDF) %>% as.matrix

      colnames(FixedXMatrix)[1] <- "Intercept"

    }else{

      f1 <- as.formula("~ 1")

      FixedXMatrix <- model.matrix(f1, data = TestDF)# %>% as.matrix

      colnames(FixedXMatrix)[1] <- "Intercept"

    }

    FixedCovar2 <- rownames(FixedEstimates)

    1:length(FixedCovar2) %>% map_dfc(function(i){

      Model$marginals.fixed[[FixedCovar2[i]]] %>% inla.rmarginal(NDraw, .) ->

        FocalEstimates

      names(FocalEstimates) <- paste0("Draw.", 1:NDraw)

      FocalEstimates %>% return

    }) %>% as.data.frame() -> FixedEstimateDF

    names(FixedEstimateDF) <- FixedCovar2 %>% str_replace_all("[(]Intercept[)]", "Intercept")

    # Putting them together

    if(length(RandomCovar)>0){

      XMatrix <- cbind(FixedXMatrix, RandomXMatrix)

      SharedNames <-
        c(names(FixedEstimateDF), names(RandomEstimateDF)) %>%
        intersect(colnames(XMatrix))

      EstimateDF <- bind_cols(FixedEstimateDF, RandomEstimateDF)[,SharedNames]

      EstimateDF %>% apply(1, function(a) list(a)) %>% map(1) -> EstimateList

      XMatrix <- XMatrix[,SharedNames]

      TMatrix <- t(XMatrix)

      if(Return == "Vector"){

        1:NDraw %>% map(~{

          EstimateList[[.x]] %*% TMatrix

        }) -> PredictionList

      }else{

        1:NDraw %>% map(~{

          EstimateList[[.x]] * TMatrix %>% t

        }) -> PredictionList

      }

    }else{

      XMatrix <- FixedXMatrix %>% as_tibble

      SharedNames <- intersect(names(FixedEstimateDF), colnames(XMatrix))

      FixedEstimateDF %<>% as_tibble

      FixedEstimateDF[,SharedNames] %>% apply(1, function(a) list(a)) %>% map(1) -> EstimateList

      XMatrix <- XMatrix[,SharedNames]

      TMatrix <- t(XMatrix)

      if(Return == "Vector"){

        1:NDraw %>% map(~{

          EstimateList[[.x]] %*% TMatrix

        }) -> PredictionList

      }else{

        1:NDraw %>% map(~{

          EstimateList[[.x]] * TMatrix %>% t

        }) -> PredictionList

      }

    }

    if(length(Ranges)>0){

      if(!class(Locations) == "matrix"){

        Locations <- as.matrix(Locations)

      }

      Projection <- inla.mesh.projector(mesh = Mesh, loc = Locations, dims = c(300, 300))

      Model$marginals.random[[Ranges]] %>% map(~inla.rmarginal(NDraw, .x)) -> WList

      if(!is.null(GroupVariable)){

        TestDF$GroupVariable <- TestDF[,GroupVariable]

        TestDF %>%
          mutate(Value = 1) %>%
          #select(GroupVariable, Value) %>%
          pivot_wider(names_from = "GroupVariable",
                      values_from = "Value") ->

          GroupVarMatrix

        GroupVarMatrix[,gtools::mixedsort(as.character(1:Groups))] ->

          GroupVarMatrix

        GroupVarMatrix[is.na(GroupVarMatrix)] <- 0

      }

      if(Return == "Vector"){

        1:NDraw %>% map(~{

          WList %>% map_dbl(.x) %>%
            split(1:Groups) %>%
            map(~inla.mesh.project(Projection, .x)) %>%

            bind_cols ->

            WPredictions

          names(WPredictions) <- paste0(Ranges, ".", names(WPredictions))

          if(!is.null(GroupVariable)){

            WPredictions*GroupVarMatrix ->

              WPredictions

          }

          FullPredictions <- PredictionList[[.x]] + rowSums(WPredictions)

          return(FullPredictions)

        }) -> FullPredictionList

      }else{

        1:NDraw %>% map(~{

          WList %>% map_dbl(.x) %>%
            split(1:Groups) %>%
            map(~inla.mesh.project(Projection, .x)) %>%

            bind_cols ->

            WPredictions

          names(WPredictions) <- paste0(Ranges, ".", names(WPredictions))

          if(!is.null(GroupVariable)){

            WPredictions*GroupVarMatrix ->

              WPredictions

          }

          FullPredictions <- PredictionList[[.x]] %>%
            as_tibble %>%
            bind_cols(WPredictions) %>%
            as.data.frame()

          return(FullPredictions)

        }) -> FullPredictionList

      }

    }else{

      FullPredictionList <- PredictionList

    }

    FullPredictionList %>% return

  }
}
