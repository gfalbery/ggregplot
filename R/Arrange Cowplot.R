
# Arrange Cowplot

ArrangeCowplot <- function(PlotList){

  Plot <- PlotList[[1]]

  for(i in 2:length(PlotList)){

    Plot <- Plot + PlotList[[i]]

  }

  return(Plot)

}
