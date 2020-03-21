
# INLA Predictions ####

INLAFit <- function(Model, TestDF,
                    FixedCovar = NULL,
                    Locations = NULL,
                    Mesh = NULL,
                    Draw = F, NDraw = 1){

  require(INLA)

  RandomEstimates <- Model$summary.random

  RandomCovar <- names(RandomEstimates) %>% setdiff("w")

  if(length(RandomCovar)>0){

    lapply(RandomCovar, function(a){

      TestDF[,a] <- as.factor(TestDF[,a])

      model.matrix(as.formula(paste0("~ -1 + ", a)), data = TestDF) -> NewDF

      colnames(NewDF) %>% str_replace_all(a, paste0(a, ".")) ->
        colnames(NewDF)

      NewDF <- NewDF %>% as.data.frame()

      return(NewDF)

    }) %>% bind_cols() %>% as.matrix -> RandomXMatrix

    if(!Draw){

      lapply(1:length(RandomCovar), function(i){

        FocalEstimates <- RandomEstimates[[RandomCovar[i]]]$mean %>%
          data.frame() %>% t %>% as.data.frame()

        names(FocalEstimates) <- paste0(RandomCovar[i], ".", RandomEstimates[[RandomCovar[i]]]$ID)

        return(FocalEstimates)

      }) %>% bind_cols %>% unlist -> RandomEstimateVector

    }else{

      lapply(1:length(RandomCovar), function(i){

        Model$marginals.random[[RandomCovar[i]]] %>% map_dbl(~inla.rmarginal(NDraw, .x)) ->

          FocalEstimates

        names(FocalEstimates) <- paste0(RandomCovar[i], ".", RandomEstimates[[RandomCovar[i]]]$ID)

        return(FocalEstimates)

      }) %>% unlist -> RandomEstimateVector

    }

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

  if(!Draw){

    FixedEstimateVector <- FixedEstimates$mean

    names(FixedEstimateVector) <- rownames(FixedEstimates) %>% str_replace_all("[(]Intercept[)]", "Intercept")

  }else{

    FixedCovar2 <- rownames(FixedEstimates)

    lapply(1:length(FixedCovar2), function(i){

      Model$marginals.fixed[[FixedCovar2[i]]] %>% inla.rmarginal(NDraw, .) ->

        FocalEstimates

    }) %>% unlist -> FixedEstimateVector

    names(FixedEstimateVector) <- FixedCovar2 %>% str_replace_all("[(]Intercept[)]", "Intercept")

  }

  # Putting them together

  if(length(RandomCovar)>0){

    XMatrix <- cbind(FixedXMatrix, RandomXMatrix)

    SharedNames <-
      intersect(names(FixedEstimateVector), names(FixedEstimateVector)) %>%
      intersect(colnames(XMatrix))

    EstimateVector <- c(FixedEstimateVector, RandomEstimateVector)[colnames(XMatrix)]

    (EstimateVector[colnames(XMatrix)] %*% t(XMatrix)) %>% c -> Predictions

  }else{

    XMatrix <- FixedXMatrix

    SharedNames <- intersect(names(FixedEstimateVector), colnames(XMatrix))

    EstimateVector <- c(FixedEstimateVector)[SharedNames]

    (EstimateVector[SharedNames] %*% t(XMatrix[,SharedNames])) %>% c ->

      Predictions

  }

  if("w" %in% names(RandomEstimates)){

    if(!class(Locations) == "matrix"){

      Locations <- as.matrix(Locations)

    }

    Projection <- inla.mesh.projector(mesh = Mesh, loc = Locations, dims = c(300, 300))

    WPredictions <- c(inla.mesh.project(Projection, Model$summary.random$w$mean))

    FullPredictions <- Predictions + WPredictions

  }else{

    FullPredictions <- Predictions

  }

  FullPredictions %>% return

}
