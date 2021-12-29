
# MakePredictDF ####

MakePredictDF <- function(DF, Length.Out = 100){
  
  PredList <- list()
  
  for(i in 1:ncol(DF)){
    
    if(class(DF[,i]) %in% c("numeric", "integer")){
      
      PredList[[i]] <- c(seq(from = min(DF[,i]),
                             to = max(DF[,i]),
                             length.out = Length.Out),
                         mean(DF[,i]))
      
    }else{
      
      PredList[[i]] <- unique(DF[,i])
        
    }
    
  }
  
  names(PredList) <- colnames(DF)
  
  return(PredList)
  
}