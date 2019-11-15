
# INLA Predictions ####

INLAPredictions(Data = SummaryList$DFList[[1]], 
                Model = ModelList[[1]][[3]], 
                Mesh = MeshList[[1]], 
                Points = SummaryList$DFList[[1]][,c("X", "Y")], 
                Response = Parasites[1], FixedCovar = SummaryList$Covar)

INLAPredictions <- function(Data, Response, FixedCovar, Model, Mesh = NULL, Points = NULL){
  
  PredictionCorDF <- data.frame(Observed = Data[,Response])
  
  TestDF <- Data %>% rownames_to_column()
  
  RandomEstimates <- Model$summary.random
  
  RandomCovar <- names(RandomEstimates) %>% setdiff("w")
  
  lapply(RandomCovar, function(a){
    
    print(a)
    
    TestDF %>% spread(a, "rowname") -> NewDF
    
    NewDF %>% select((ncol(TestDF)+1):ncol(NewDF)) %>%
      mutate_all(function(b) 1 - as.numeric(is.na(b))) -> NewDF
    
    colnames(NewDF) <- paste0(a, ".", colnames(NewDF))
    
    return(NewDF)
    
  }) %>% bind_cols() %>% as.matrix -> RandomXMatrix
  
  # Fixed effects ####
  
  FixedEstimates <- Model$summary.fixed
  
  f1 <- as.formula(paste0("~ 1 +", paste0(FixedCovar, collapse = " + ")))
  
  FixedXMatrix <- model.matrix(f1, data = TestDF) %>% as.matrix
  
  colnames(FixedXMatrix)[1] <- "Intercept"
  
  # Putting them together 
  
  XMatrix <- cbind(FixedXMatrix, RandomXMatrix)
  
  FixedEstimateVector <- FixedEstimates$mean
  
  names(FixedEstimateVector) <- rownames(FixedEstimates)
  
  lapply(1:length(RandomCovar), function(i){
    
    FocalEstimates <- RandomEstimates[[RandomCovar[i]]]$mean %>% 
      data.frame() %>% t %>% as.data.frame()
    
    names(FocalEstimates) <- paste0(RandomCovar[i],".",RandomEstimates[[RandomCovar[i]]]$ID)
    
    return(FocalEstimates)
    
  }) %>% bind_cols %>% unlist -> RandomEstimateVector
  
  EstimateVector <- c(FixedEstimateVector, RandomEstimateVector)[colnames(XMatrix)]
  
  (EstimateVector[colnames(XMatrix)] %*% t(XMatrix)) %>% c -> Predictions
  
  if("w" %in% names(RandomEstimates)){
    
    Projection <- inla.mesh.projector(Mesh, as.matrix(Points), dims = c(300, 300))
    WPredictions <- c(inla.mesh.project(Projection, Model$summary.random$w$mean))
    
    FullPredictions <- Predictions + WPredictions
    
  }else{
    
    FullPredictions <- Predictions
    
  }
  
  PredictionCorDF$Predicted <- FullPredictions
  
  return(PredictionCorDF)
  
}
