
SmoothOutput <-
  function(Data,
           Model,
           Covariates, Response = "y",
           OutputCovariates,
           HoldFixed = NULL,
           Output = "Link", Family = "Gaussian",
           LineAlpha = 0.05,
           AddPoints = F, TestDF = NULL, PointAlpha = 1, PointColour = "black",
           AddP = F, AddEstimate = F, LimitClip = T, TextColour = NA,
           ManualPY = NULL,
           XTransform = NULL, YTransform = NULL, XAdd = NULL, YAdd = NULL,
           SmoothFillAlpha = 0.1, SmoothColour = AlberColours[[1]],
           ReturnPlot = F,
           ...){

    OutputList <- list()

    i <- 1

    if(Family == "Binomial" & Output == "Data") ManualPY <- 1.1

    if(any(class(Model) %in% c("bam", "gam"))){

      for(i in 1:length(OutputCovariates)){

        print(OutputCovariates[i])

        PredList <- MakePredictDF(Data[,Covariates],
                                  HoldNumeric = Covariates %>% setdiff(OutputCovariates[i]),
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

        PredDF$x <- PredDF[,OutputCovariates[i]]

        FocalPlot <-
          PredDF %>%
          ggplot(aes(x, Value))

        if(AddPoints){

          TestDF$x <- TestDF[,OutputCovariates[i]]

          FocalPlot <- FocalPlot + geom_point(data = TestDF, aes(x, y), alpha = PointAlpha)

        }

        FocalPlot <-
          FocalPlot +
          geom_ribbon(aes(ymin = Lower, ymax = Upper),
                      alpha = SmoothFillAlpha, colour = SmoothColour) +
          geom_line() +
          labs(x = OutputCovariates[i], y = Response)

        if(LimitClip){

          FocalPlot <- FocalPlot + lims(y = c(min(PredDF$Lower), max(PredDF$Upper)) +
                                          c(-diff(range(PredDF$Value))*0.1,
                                            diff(range(PredDF$Value))*0.1))

        }

        OutputList[[i]] <- FocalPlot

      }

    } else if(any(class(Model) %in% c("inla"))){

      i <- 1

      Model %>%
        INLAFit(TestDF = Data,
                FixedCovar = Covariates,
                # HoldFixed = HoldFactors,
                ...,
                NDraw = 100, Draw = T) %>% map_dbl(mean) -> Intercepts

      for(i in 1:length(OutputCovariates)){

        print(OutputCovariates[i])

        # PredList <- MakePredictDF(Data[,Covariates[!str_detect(Covariates, ":")]],
        #                           HoldNumeric = Covariates[!str_detect(Covariates, ":")] %>% setdiff(OutputCovariates[i]),
        #                           HoldFactor = HoldFactors)
        #
        # PredDF <- PredList %>% expand.grid()

        X = seq(min(Data[,OutputCovariates[i]]),
                max(Data[,OutputCovariates[i]]),
                length.out = 100) %>% c

        Model %>%
          GetEstimates(OutputCovariates[i], NDraw = 100, Draws = T) -> Slopes

        B = mean(Slopes)
        A = mean(Intercepts)
        Y = A + B*X

        1:length(Slopes) %>%
          map(~data.frame(X = X,
                          Y = X*Slopes[[.x]] + Intercepts[[.x]])) -> SlopeDF

        SlopeDF %<>% bind_rows(.id = "Rep")

        FitLine <-
          data.frame(
            X = X,
            Y = Y
          )

        Data$X <- Data[,OutputCovariates[i]]
        Data$Y <- Data[,Response]

        if(Output == "Data" & Family == "Binomial"){

          FitLine %<>% mutate_at(c("Y"), logistic)
          SlopeDF %<>% mutate_at(c("Y"), logistic)

        }

        if(Output == "Data" & Family == "NBinomial"){

          FitLine %<>% mutate_at(c("Y"), ~exp(.x))
          SlopeDF %<>% mutate_at(c("Y"), ~exp(.x))

        }

        if(!is.null(XTransform)){

          FitLine %<>% mutate_at(c("X"), ~.x*XTransform)
          SlopeDF %<>% mutate_at(c("X"), ~.x*XTransform)

        }

        if(!is.null(YTransform)){

          FitLine %<>% mutate_at(c("Y"), ~.x*YTransform)
          SlopeDF %<>% mutate_at(c("Y"), ~.x*YTransform)

        }

        if(!is.null(XAdd)){

          FitLine %<>% mutate_at(c("X"), ~.x + XAdd)
          SlopeDF %<>% mutate_at(c("X"), ~.x + XAdd)

        }

        if(!is.null(YAdd)){

          FitLine %<>% mutate_at(c("Y"), ~.x + YAdd)
          SlopeDF %<>% mutate_at(c("Y"), ~.x + YAdd)

        }

        FocalPlot <-
          FitLine %>%
          ggplot(aes(X, Y))

        if(AddPoints){

          TestDF$x <- TestDF[,OutputCovariates[i]]
          TestDF$y <- TestDF[,Response]

          if(!is.null(XTransform)){

            TestDF %<>% mutate_at(c("x"), ~.x*XTransform)

          }

          if(!is.null(YTransform)){

            TestDF %<>% mutate_at(c("y"), ~.x*YTransform)

          }

          if(!is.null(XAdd)){

            TestDF %<>% mutate_at(c("x"), ~.x + XAdd)

          }

          if(!is.null(YAdd)){

            TestDF %<>% mutate_at(c("y"), ~.x + YAdd)

          }

          if(Output == "Data"|Family == "Gaussian"){

            FocalPlot <- FocalPlot + geom_point(data = TestDF, aes(x, y),
                                                alpha = PointAlpha, colour = PointColour)

          }else if(Output == "Link"){

            if(Family == "Binomial"){

              FocalPlot <- FocalPlot + geom_point(data = TestDF, aes(x, logit(y)),
                                                  alpha = PointAlpha, colour = PointColour)

            }else{

              FocalPlot <- FocalPlot + geom_point(data = TestDF, aes(x, log(y + 1)),
                                                  alpha = PointAlpha, colour = PointColour)

            }

          }

        }

        FocalPlot <-
          FocalPlot +
          geom_line(alpha = LineAlpha, data = SlopeDF, aes(X, Y, group = Rep)) +
          geom_line(data = FitLine, size = 1)  +
          labs(y = Response, x = OutputCovariates[i])

        if(AddP | AddEstimate){

          if(AddPoints == T & LimitClip == F){

            LabelYMax <- max(TestDF$y) + diff(range(SlopeDF$Y))*0.05

          }else{

            LabelYMax <- max(SlopeDF$Y) + diff(range(SlopeDF$Y))*0.05

          }

          LabelDF <- data.frame(X = mean(range(FitLine$X)),
                                Y = LabelYMax)

          # if(!is.null(XTransform)){
          #
          #   LabelDF %<>% mutate_at(c("X"), ~.x*XTransform)
          #
          # }
          #
          # if(!is.null(YTransform)){
          #
          #   LabelDF %<>% mutate_at(c("Y"), ~.x*YTransform)
          #
          # }

          if(AddP){

            PValue <- INLAPValue(Model, OutputCovariates[i])[[1]]

            if(PValue < 0.001){

              PValue <- "P < 0.001"

            }else{

              PValue <- paste0("P = ", round(PValue, 3))

            }

            LabelDF$PValue <- LabelDF$Label <- PValue

          }

          if(AddEstimate){

            if(is.null(XTransform)) XTransform <- 1
            if(is.null(YTransform)) YTransform <- 1

            Estimate <-
              GetEstimates(Model, OutputCovariates[i], Mode = "Numeric")/
              XTransform*YTransform

            Estimate %<>% round(3)

            Estimate <- paste0(Estimate[1], " (", Estimate[2], ", ", Estimate[3], ")")

            LabelDF$Estimate <- LabelDF$Label <- Estimate

          }

          if(AddP & AddEstimate){

            LabelDF$Label <- paste0(LabelDF$Estimate, "; ", LabelDF$PValue)

          }

          if(!is.null(ManualPY)){

            LabelDF$Y <- ManualPY

          }

          FocalPlot <-
            FocalPlot +
            geom_label(data = LabelDF, aes(label = Label, x = X, y = Y),
                       fill = "white", label.size = NA,
                       colour = TextColour)

        }

        if(LimitClip){

          FocalPlot <- FocalPlot + lims(y = range(SlopeDF$Y) +
                                          c(-diff(range(SlopeDF$Y))*0.1,
                                            diff(range(SlopeDF$Y))*0.1))

        }

        OutputList[[i]] <- FocalPlot

      }
    }

    if(ReturnPlot){

      OutputList %>% ArrangeCowplot() %>% plot

    }

    return(OutputList)

  }
