
# Generates all possible combinations of a set of covariates ####

FormulaGen <- function(Response, covar){
  
  unlist(lapply(length(covar):0, function(x){
    
    formuladf <- as.data.frame(combn(covar,x))
    
    if(x>0){
      unlist(lapply(formuladf, function(y) as.formula(paste(Response, " ~ ", paste(y, collapse = " + "), " + ", 
                                                            ifelse(Response %in% c("ScaleTotA","ScaleTcA","lEPG"), 
                                                                   paste(c("Autumn","Spring","Age","Year2016", "Year2017"), collapse = " + "), 
                                                                   paste(c("Age","Year2016","Year2017"), collapse = " + "))))))
    }else{
      as.formula(ifelse(Response %in% c("ScaleTotA","ScaleTcA","lEPG"), 
             paste0(Response," ~ Age + Autumn + Spring + Year2016 + Year2017"),
             paste0(Response," ~ Age + Year2016 + Year2017")))
    }
  }
  ))
}

