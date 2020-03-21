
INLAPValue <- function(Model, Vars = NULL, Decimals = 6){
  
  if(is.null(Vars)){
    
    Vars <- names(Model$marginals.fixed)
    
  }
  
  Vars %>% map(~Model$marginals.fixed[[.x]] %>% 
                 
                 inla.rmarginal(marginal = ., 10^Decimals) %>% 
                 
                 PCalc %>% return
                 
                 ) -> Return

  names(Return) <- Vars
  
  return(Return)
  
}