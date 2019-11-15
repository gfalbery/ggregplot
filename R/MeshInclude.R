
# MeshInclude #####

MeshInclude <- function(xvar, yvar, xmin, xmax, ymin, ymax){
  
  ifelse(xvar>xmin&
           xvar<xmax&
           yvar>ymin&
           yvar<ymax,
         1,
         0)
  
}
