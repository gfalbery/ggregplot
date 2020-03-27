
# Generates all possible combinations of a set of Covariates ####

FormulaGen <- function(Response, Covar, InterceptOnly = T){

  1:length(Covar) %>%
    map(~combn(Covar, .x) %>% apply(2, list) %>% unlist(recursive = F)) %>%
    unlist(recursive = F) ->

    CovarList

  CovarList %>% map(~paste0(Response, " ~ ", paste(.x, collapse = " + ")) %>% as.formula) -> FormulaList

  if(InterceptOnly) FormulaList %>% append(paste0(Response, " ~ 1") %>% as.formula %>% list, .) -> FormulaList

  return(FormulaList)

}


