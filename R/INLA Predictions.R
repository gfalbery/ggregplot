
# INLA Predictions ####

INLAFit <- function(Model, TestDF,
                    FixedCovar = NULL,
                    Locations = NULL,
                    Mesh = NULL){

  require(INLA)

  RandomEstimates <- Model$summary.random

  RandomCovar <- names(RandomEstimates) %>% setdiff("w")

  if(length(RandomCovar)>0){

    lapply(RandomCovar, function(a){

      model.matrix(as.formula(paste0("~ -1 + ", a)), data = TestDF) -> NewDF

      colnames(NewDF) %>% str_replace_all(a, paste0(a, ".")) ->
        colnames(NewDF)

      NewDF <- NewDF %>% as.data.frame()

      return(NewDF)

    }) %>% bind_cols() %>% as.matrix -> RandomXMatrix

    lapply(1:length(RandomCovar), function(i){

      FocalEstimates <- RandomEstimates[[RandomCovar[i]]]$mean %>%
        data.frame() %>% t %>% as.data.frame()

      names(FocalEstimates) <- paste0(RandomCovar[i], ".", RandomEstimates[[RandomCovar[i]]]$ID)

      return(FocalEstimates)

    }) %>% bind_cols %>% unlist -> RandomEstimateVector

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

  # Putting them together

  if(length(RandomCovar)>0){

    XMatrix <- cbind(FixedXMatrix, RandomXMatrix)

    FixedEstimateVector <- FixedEstimates$mean

    names(FixedEstimateVector) <- rownames(FixedEstimates)

    EstimateVector <- c(FixedEstimateVector, RandomEstimateVector)[colnames(XMatrix)]

    (EstimateVector[colnames(XMatrix)] %*% t(XMatrix)) %>% c -> Predictions

  }else{

    XMatrix <- FixedXMatrix

    FixedEstimateVector <- FixedEstimates$mean

    names(FixedEstimateVector) <- rownames(FixedEstimates)

    EstimateVector <- c(FixedEstimateVector)[colnames(XMatrix)]

    (EstimateVector[colnames(XMatrix)] %*% t(XMatrix)) %>% c ->

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
