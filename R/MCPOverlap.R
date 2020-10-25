
# Overlap between polygons ####

MCPOverlap <- function(CPs, Dyad, Symmetrical = T){
  
  p1 <- CPs[[Dyad[1]]]
  p2 <- CPs[[Dyad[2]]]
  
  Intersect <- gIntersection(p1,p2)
  
  if(!is.null(Intersect)){
    
    Numerator <- gArea(Intersect)
    
    if(Symmetrical){
      
      Denominator <- gArea(p1) + gArea(p2) - Numerator
      
    }else{
      
      Denominator <- gArea(p1)
      
    }
    
    Overlap <- Numerator / Denominator
    
  }else Overlap <- 0
  
  return(Overlap)
  
}