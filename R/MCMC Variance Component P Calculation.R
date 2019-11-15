PCalc<-function(Vector){
  table(Vector>0)
  min(table(Vector>0))/(length(Vector)/2)
}
