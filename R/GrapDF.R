
GrapDF <- function(List, Var){
  
  Mapped <- List %>% map(Var)
  
  N <- 1:length(Mapped)
  
  IsNull <- Mapped %>% sapply(is.null)
  
  data.frame(
    
    N = N[!IsNull],
    
    Mapped[!IsNull] %>% unlist
    
  )
}

