
AnnualChange <- function(Matrix){
  
  N <- nrow(Matrix)
  
  Matrix[2:N, 1:(N-1)]
  
  2:N %>% map_dbl(~Matrix[.x, .x-1])
  
}
