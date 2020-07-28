
# Interpolated Median ####

# Taken from http://aec.umich.edu/median.php

#  Define variables as follows:
#  M = the standard median of the responses
#  nl = number of responses strictly less than M
#  ne = number of responses equal to M
#  ng = number of responses strictly greater than M
#  The interpolated median IM is then computed as follows:
#  If ne is nonzero:
#  IM = M + (ng - nl) / (2ne)
#  If ne is zero:
#  IM = M

InterpolatedMedian <- function(X){

  M <- median(X)
  nl <- X[X < M] %>% length
  ne <- X[X == M] %>% length
  ng <- X[X > M] %>% length

  nl <- as.numeric(X < M)
  ne <- as.numeric(X == M)
  ng <- as.numeric(X > M)

  if(ne == 0){

    IM <- M + (ng - nl)/2*ne

  }else{

    IM <- M

  }

  return(IM)

}
