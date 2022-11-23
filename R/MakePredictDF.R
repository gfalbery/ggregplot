
# MakePredictDF ####

MakePredictDF <- function(DF,
                          Length.Out = 100,
                          HoldNumeric = NULL,
                          HoldFactor = NULL){



  PredList <- list()

  if(length(HoldNumeric) == 0){

    HoldNumeric <- NULL

  }

  NumericDF <- DF %>%
    dplyr::select_if(is.numeric)

  if(ncol(NumericDF) > 0){

    for(i in 1:ncol(NumericDF)){

      PredList[[i]] <- c(seq(from = min(NumericDF[,i]),
                             to = max(NumericDF[,i]),
                             length.out = Length.Out),
                         mean(NumericDF[,i]))

    }

    names(PredList) <- colnames(NumericDF)

    if(length(intersect(HoldNumeric, names(PredList))) > 0){

      intersect(HoldNumeric, names(PredList)) %>%
        map(function(a){

          PredList[[a]] <<- last(PredList[[a]])

        })
    }
  }

  FactorDF <- DF %>% dplyr::select_if(~is.character(.x)|is.factor(.x))

  if(ncol(FactorDF) > 0){

    for(i in 1:ncol(FactorDF)){

      PredList[[length(PredList) + 1]] <- c(unique(FactorDF[,i]))

    }

    names(PredList)[(ncol(NumericDF) + 1):length(PredList)] <- c(colnames(FactorDF))

    if(length(intersect(names(HoldFactor), names(PredList))) > 0){

      intersect(names(HoldFactor), names(PredList)) %>%

        map(function(a){

          PredList[[a]] <<- HoldFactor[[a]]

        })
    }
  }

  return(PredList)

}
