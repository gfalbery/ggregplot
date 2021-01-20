ProportionalMatrix <- function(Matrix, Observations){
  
  N <- Matrix %>% nrow
  
  A <- rep(Observations, each = N)
  B <- rep(Observations, N)
  
  AMatrix <- matrix(A, ncol = N)
  BMatrix <- matrix(B, ncol = N)
  
  Matrix/(AMatrix + BMatrix - Matrix)
  
}