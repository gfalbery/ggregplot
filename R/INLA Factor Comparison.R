INLAFactorComp <- function(
    Model,
    Data,
    Factor,
    Reference = NULL,
    NDraws = 10000,
    CredibleInterval = 0.95
) {
  
  stopifnot(
    Factor %in% names(Data)
  )
  
  FactorValues <- factor(Data[[Factor]])
  
  if(!is.null(Reference)) {
    FactorValues <- relevel(FactorValues, ref = Reference)
  }
  
  FactorLevels <- levels(FactorValues)
  ReferenceLevel <- FactorLevels[1]
  
  FixedNames <- rownames(Model$summary.fixed)
  Alpha <- (1 - CredibleInterval) / 2
  
  LevelDraws <- setNames(
    vector("list", length(FactorLevels)),
    FactorLevels
  )
  
  LevelDraws[[ReferenceLevel]] <- rep(0, NDraws)
  
  for(Level in setdiff(FactorLevels, ReferenceLevel)) {
    
    CandidateNames <- unique(c(
      paste0(Factor, Level),
      paste0(Factor, make.names(Level)),
      paste0(make.names(Factor), make.names(Level))
    ))
    
    CoefficientName <- CandidateNames[
      CandidateNames %in% FixedNames
    ][1]
    
    if(is.na(CoefficientName)) {
      stop(
        "Could not find coefficient for level: ",
        Level,
        "\nAvailable fixed effects:\n",
        paste(FixedNames, collapse = ", ")
      )
    }
    
    Marginal <- Model$marginals.fixed[[CoefficientName]]
    
    LevelDraws[[Level]] <- INLA::inla.rmarginal(
      NDraws,
      Marginal
    )
  }
  
  Comparisons <- combn(
    FactorLevels,
    2,
    simplify = FALSE
  )
  
  purrr::map_dfr(
    Comparisons,
    function(LevelPair) {
      
      Level1 <- LevelPair[1]
      Level2 <- LevelPair[2]
      
      Difference <-
        LevelDraws[[Level2]] -
        LevelDraws[[Level1]]
      
      Ratio <- exp(Difference)
      
      tibble::tibble(
        Level1 = Level1,
        Level2 = Level2,
        MeanDifference = mean(Difference),
        MedianDifference = median(Difference),
        LowerDifference = quantile(Difference, Alpha),
        UpperDifference = quantile(Difference, 1 - Alpha),
        MeanRatio = mean(Ratio),
        MedianRatio = median(Ratio),
        LowerRatio = quantile(Ratio, Alpha),
        UpperRatio = quantile(Ratio, 1 - Alpha),
        ProbabilityLevel2Greater = mean(Difference > 0),
        ProbabilityLevel2Lower = mean(Difference < 0),
        PosteriorP = 2 * min(
          mean(Difference > 0),
          mean(Difference < 0)
        )
      ) %>% data.frame
    }
  )
}