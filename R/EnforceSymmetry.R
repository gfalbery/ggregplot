
EnforceSymmetry <- function(Matrix, From = "Lower"){
  
  if(From == "Upper"){
    
    Matrix[lower.tri(Matrix, diag = F)] <- 
      
      t(Matrix)[!upper.tri(t(Matrix), diag = T)]
    
    
  }else{
    
    Matrix[upper.tri(Matrix, diag = F)] <- 
      
      t(Matrix)[!lower.tri(t(Matrix), diag = T)]
    
  }
  
  return(Matrix)

}
