
 GristMatrix <- function(Edgelist, ColNames = NULL){
   
   Matrix <- dist(Edgelist) %>% as.matrix
   
   if(!is.null(ColNames)){ 
     
     dimnames(Matrix) <- list(ColNames, ColNames)
     
   }
   
   Matrix %>% return
   
 }
   