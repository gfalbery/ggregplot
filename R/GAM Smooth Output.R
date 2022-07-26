
SmoothOutput <-
  function(Data,
           Model,
           Covariates, Response = "y",
           OutputCovariates,
           HoldFactors,
           Output = "Link",
           AddPoints = F, TestDF = NULL, PointAlpha = 1,
           ...){

    OutputList <- list()

    i <- 1

    for(i in 1:length(OutputCovariates)){

      print(OutputCovariates[i])

      PredList <- MakePredictDF(Data[,Covariates],
                                HoldNumeric = Covariates %>% setdiff(OutputCovariates[i]),
                                HoldFactor = HoldFactor)

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

      PredDF$x <- PredDF[,OutputCovariates[i]]

      FocalPlot <-
        PredDF %>%
        ggplot(aes(x, Value)) +
        geom_ribbon(aes(ymin = Lower, ymax = Upper),
                    alpha = 0.1, colour = AlberColours[1]) +
        geom_line() +
        labs(x = OutputCovariates[i], y = "Fit")

      if(AddPoints){

        TestDF$x <- TestDF[,OutputCovariates[i]]

        FocalPlot <- FocalPlot + geom_point(data = TestDF, aes(x, y), alpha = PointAlpha)

      }

      OutputList[[i]] <- FocalPlot

    }

    return(OutputList)

  }
