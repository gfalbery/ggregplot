PCalc <- function(Vector, Minimum = "Minimum"){

  table(Vector > 0)

  Value <- min(table(Vector > 0))/
    (length(Vector)/2)

  if(Value == 2){

    if(Minimum == "Minimum"){

      Value <- 1/length(Vector)

    }else if(Minimum == "Zero"){

      Value <- 0

    }
  }

  return(Value)

}
