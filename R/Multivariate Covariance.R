MultivCovariance <- function(Model){
  Model$VCV %>% as.data.frame %>% apply(2, function(a){
    a %>% as.mcmc %>% HPDinterval
  }) %>% t -> Intervals
  Model$VCV %>% as.data.frame %>% apply(2, function(a){
    a %>% as.mcmc %>% posterior.mode
  }) %>% t -> Points
  Intervals %>% as.data.frame %>% rownames_to_column %>%
    rename(Lower = V1, Upper = V2) %>% mutate(Mode = c(Points)) %>%
    mutate(Component = rowname %>% str_split("[.]") %>% map_chr(last)) %>%
    mutate(Vars = rowname %>% str_split("[.]") %>% map_chr(~.x[1:(length(.x) - 1)] %>% paste0(collapse = "."))) %>%
    # separate(rowname, "[.]", into = c("Vars", "Component")) %>%
    mutate(Vars = Vars %>% str_remove_all("trait")) %>%
    separate(Vars, ":", into = c("Var1", "Var2")) -> EstimateDF
  Matrix <- matrix(1:(nunique(EstimateDF$Var1)^2), ncol = nunique(EstimateDF$Var1))
  Rows <- Matrix[lower.tri(Matrix, diag = FALSE)]
  EstimateDF %>%
    mutate(Rowname = rep(1:max(c(Matrix)), n()/max(c(Matrix)))) %>%
    mutate(Keep = as.numeric(Rowname%in%Rows)) ->
    EstimateDF
  EstimateDF %>% filter(Var1 == Var2) -> VarianceDF
  EstimateDF %>% filter(Var1 != Var2, Keep == 1) -> CovarianceDF
  return(CovarianceDF)
}
