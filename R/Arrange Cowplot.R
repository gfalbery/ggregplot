
# Arrange Cowplot

ArrangePatchwork <- function(PlotList){

  Plot <- PlotList[[1]]

  for(i in 2:length(PlotList)){

    Plot <- Plot + PlotList[[i]]

  }

  return(Plot)

}

ArrangeCowplot <- function(PlotList){

  Plot <- PlotList[[1]]

  for(i in 2:length(PlotList)){

    Plot <- Plot + PlotList[[i]]

  }

  return(Plot)

}
