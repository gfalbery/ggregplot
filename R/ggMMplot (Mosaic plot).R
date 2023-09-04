############ ggMMplot ##############

ggMMplot  <-  function(DF, Var1, Var2, Alpha = 1, Just = F){

  require(ggplot2)

  if(Just){

    Angle  <-  45
    HJust  <-  0

  }else{

    Angle  <-  0
    HJust  <-  0.5

  }

  DF2 <- na.omit(DF[,c(Var1,Var2)])

  DF2[,Var1] <- as.factor(DF2[,Var1])
  DF2[,Var2] <- as.factor(DF2[,Var2])

  levVar1  <-  length(levels(DF2[,Var1]))
  levVar2  <-  length(levels(DF2[,Var2]))

  jointTable  <-  prop.table(table(DF2[,Var1], DF2[,Var2]))
  plotData  <-  as.data.frame(jointTable)

  plotData$marginVar1  <-  prop.table(table(DF2[,Var1]))

  plotData$Var2Height  <-  plotData$Freq / plotData$marginVar1

  plotData$Var1Center  <-  c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2

  plotData$N <- c(table(DF2[,Var1],DF2[,Var2]))

  ggplot(plotData, aes(Var1Center, Var2Height)) +
    geom_col(#stat = "identity",
      width = plotData$marginVar1,
      aes(#width = marginVar1,
        fill = Var2),
      col = "Black", alpha = Alpha) +
    geom_text(aes(label = as.character(Var1),
                  hjust = HJust, angle = Angle,
                  x = Var1Center, y = 1.05)) +
    labs(x = NULL,y = NULL, fill = Var2)

}
