############# BarBarGraph

# data_summary <- function(data, varname, groupnames){
#   # require(plyr)
#   summary_func <- function(x, col){
#     c(mean = mean(x[[col]], na.rm = TRUE),
#       sd = sd(x[[col]], na.rm = TRUE),
#       N = length(x[[col]]),
#       se = sd(x[[col]], na.rm = TRUE)/sqrt(length(x[[col]])))
#   }
#
#   data_sum<-ddply(data, groupnames, .fun = summary_func,
#                   varname)
#   data_sum <- rename(data_sum, c("mean" = varname))
#   return(data_sum)
# }

BarBarGraph <- function(Data, group, x, y, AddPoints = F){

  require(magrittr)

  BarFun <- mean

  Data %<>% as.data.frame

  Data <- Data[,c(x,y, group)] %>% na.omit()

  Data$X <- Data[,x]
  Data$Y <- Data[,y]
  Data$Colour <- Data[,group]

  if(!is.factor(Data[,group])){

    Data$Colour %<>% factor(levels = sort(unique(Data[,group])))

  }

  Errordf <- Data %>% group_by(Colour) %>%
    summarise(MeanY = BarFun(Y, na.rm = T),
              sdY = sd(Y, na.rm = T),
              MeanX = BarFun(X, na.rm = T),
              sdX = sd(X, na.rm = T),
              N = n()) %>%
    mutate(seY = sdY/(N^0.5)) %>%
    mutate(YLower = MeanY - seY,
           YUpper = MeanY + seY) %>%
    mutate(seX = sdX/(N^0.5)) %>%
    mutate(XLower = MeanX - seX,
           XUpper = MeanX + seX) %>%
    as.data.frame() %>%
    arrange(desc(MeanX), desc(MeanY)) %>% na.omit()

  SPlot <- Data %>%
    ggplot(aes(X, Y, colour = Colour)) +
    labs(x = x, y = y, colour = group)

  if(AddPoints){

    SPlot <- SPlot + geom_point()

  }

  SPlot <- SPlot +

    geom_errorbar(data = Errordf, inherit.aes = F,
                  aes(x = MeanX,
                      ymin = YLower,
                      ymax = YUpper,
                      group = Colour),
                  # width = 0.1,
                  colour = "black") +

    geom_errorbarh(data = Errordf, inherit.aes = F,
                   aes(y = MeanY,
                       xmin = XLower,
                       xmax = XUpper,
                       group = Colour),
                   # width = 0.1,
                   colour = "black")

  SPlot %>% return

}

