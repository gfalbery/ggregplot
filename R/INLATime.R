
INLATime <- function(ModelList, Fun = "Each"){
  
  if(class(ModelList) == "list"){
    
    Times <- ModelList %>% map_dbl(
      
      ~.x$cpu.used[["Total"]]
      
    )
    
  }else{
    
    Times <- ModelList$cpu.used[["Total"]]
    
  }
  
  if(Fun == "Sum"){
    
    Times <- sum(Times)
    
  }
  
  return(Times)
  
}
