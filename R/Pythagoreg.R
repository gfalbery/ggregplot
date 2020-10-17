
Pythagoreg <- function(Data, X = c("X1", "X2"), Y = c("Y1", "Y2")){
  
  Data <- as.data.frame(Data)
  
  X <- abs(Data[,X[1]] - Data[,X[2]])
  Y <- abs(Data[,Y[1]] - Data[,Y[2]])
  
  Distance <- (X^2 + Y^2)^0.5
  
  return(Distance)
  
}
