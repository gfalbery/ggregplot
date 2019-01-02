############ ggMMplot ##############
ggMMplot <- function(df, var1, var2){
  require(ggplot2)
  df2<-na.omit(df[,c(var1,var2)])
  
  df2[,var1]<-as.factor(df2[,var1])
  df2[,var2]<-as.factor(df2[,var2])
  
  levVar1 <- length(levels(df2[,var1]))
  levVar2 <- length(levels(df2[,var2]))
  
  jointTable <- prop.table(table(df2[,var1], df2[,var2]))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(df2[,var1]))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2
  plotData$N<-c(table(df2[,var1],df2[,var2]))
  
  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = Var2), col = "Black") +
    geom_text(aes(label = as.character(Var1), x = var1Center, y = 1.05))+labs(x=NULL,y=NULL,fill=var2)+ THEME

  }
