ProportionalMatrix <- function(Matrix, Observations, RemoveDiagonal = T){

  if(RemoveDiagonal) diag(Matrix) <- 0

  N <- Matrix %>% nrow

  A <- rep(Observations, each = N)
  B <- rep(Observations, N)

  AMatrix <- matrix(A, ncol = N)
  BMatrix <- matrix(B, ncol = N)

  Matrix/(AMatrix + BMatrix - Matrix)

}
