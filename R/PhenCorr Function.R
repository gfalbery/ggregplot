# Phenotypic Correlation Calculation from a multivariate Model

PhenCorr <- function(Model, Terms, Exclude = NULL){

  Terms # This is the cumulative number of random and residual terms you have (variance components).
  # So if you have one random effect and one residual (units?) term that means Terms = 2.

  VCV <- Model$VCV %>% as.data.frame()

  if(!is.null(Exclude)){

    VCV <- VCV %>% dplyr::select(!ends_with(Exclude))

  }

  matrix <- matrix(1:(dim(VCV)[2]/Terms), ncol=(dim(VCV)[2]/Terms)^0.5)

  PCovariance <- data.frame(VCV)[,1:max(matrix)]

  for(x in 1:dim(matrix)[1]){

    for(y in 1:dim(matrix)[1]){

      PCovariance[,matrix[x,y]] <-

        as.mcmc(rowSums(VCV[,matrix[x,y] + (max(matrix)*0:(Terms-1))])/
                  rowSums(VCV[,matrix[x,x] + (max(matrix)*0:(Terms-1))] +
                            VCV[,matrix[y,y] + (max(matrix)*0:(Terms-1))])^0.5)

    }
  }

  Rows <- matrix[lower.tri(matrix,diag=FALSE)]

  dat <- data.frame(

    LCI = as.numeric(round(apply(PCovariance, 2, function(x) HPDinterval(as.mcmc(x))[1]),3)),
    Mode = as.numeric(round(apply(PCovariance, 2, function(x) posterior.mode(as.mcmc(x))),3)),
    UCI = as.numeric(round(apply(PCovariance, 2, function(x) HPDinterval(as.mcmc(x))[2]),3)),
    PMCMC = apply(PCovariance,2,function(x) PCalc(as.mcmc(x)))

  )[Rows,]

  return(dat)

}

