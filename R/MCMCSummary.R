
MCMCSummary <- function(X){

  data.frame(
    Mean = mean(X),
    Lower = HPDinterval(as.mcmc(X))[1],
    Upper = HPDinterval(as.mcmc(X))[2],
    P = PCalc(X)

  ) %>% list
}
