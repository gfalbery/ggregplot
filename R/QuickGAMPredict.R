
QuickGAMPredict <-
  
  function(Data,
           Model,
           Covariates, #Response = "y",
           OutputCovariate,
           HoldFactors = c(" " = " "),
           Output = "Link", Family = "Gaussian",
           # LineAlpha = 0.05,
           # AddPoints = F, TestDF = NULL, PointAlpha = 1, PointColour = NA,
           # AddP = F, AddEstimate = F, LimitClip = T, TextColour = NA,
           # SmoothFillAlpha = 0.1, SmoothColour = AlberColours[[1]],
           # ReturnPlot = F,
           ...){
    
    OutputList <- list()
    
    i <- 1
    
    if(any(class(Model) %in% c("bam", "gam"))){
      
      PredList <- MakePredictDF(Data[,Covariates],
                                HoldNumeric = Covariates %>% setdiff(OutputCovariate),
                                HoldFactor = HoldFactors)
      
      PredDF <- PredList %>% expand.grid()
      
      PredValues <- predict.gam(Model,
                                newdata = PredDF,
                                se.fit = T)
      
      PredDF[,c("Fit", "SE")] <- PredValues %>% bind_cols %>% as.data.frame()
      
      PredDF %<>% mutate(Value = Fit,
                         Lower = Fit - SE,
                         Upper = Fit + SE)
      
      if(Output == "Logistic"){
        
        PredDF %<>% mutate_at(c("Value", "Lower", "Upper"), logistic)
        
      }
      
      PredDF$x <- PredDF[,OutputCovariate]
      
    }
    
    PredDF %>% return
    
  }