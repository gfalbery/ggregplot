
SinaGraph <- function(df, x, y, z = x,
                      Order = F,
                      Just = F,
                      Alpha = 1,
                      Scale = "area",
                      Mean = T,
                      ColourGroups = F){

  require(ggplot2); require(dplyr); require(ggforce)

  df <- df[,c(x,y,z)] %>% na.omit()

  df$X <- df[,x]
  df$Y <- df[,y]
  df$Colour <- df[,z]

  if(ColourGroups){
    Errordf <- df %>% group_by(X, Colour) %>%
      summarise(Mean = mean(Y),
                sd = sd(Y),
                N = n()) %>%
      mutate(se = sd/(N^0.5)) %>%
      as.data.frame() %>%
      slice(order(Mean, decreasing = T))
  } else {
    Errordf <- df %>% group_by(X) %>%
      summarise(Mean = mean(Y),
                sd = sd(Y),
                N = n()) %>%
      mutate(se = sd/(N^0.5)) %>%
      as.data.frame() %>%
      slice(order(Mean, decreasing = T))
  }

  SPlot <- df %>%
    ggplot(aes(as.factor(X), Y, colour = as.factor(Colour))) +
    geom_sina(position = position_dodge(0.9),
              alpha = Alpha,
              scale = Scale) +
    labs(x = x, y = y)

  if(Mean){

    if(ColourGroups){

      SPlot <- SPlot +
        geom_point(data = Errordf,
                   aes(y = Mean, group = as.factor(Colour)),
                   colour = "black",
                   position = position_dodge(0.9)) +
        geom_errorbar(data = Errordf, inherit.aes = F,
                      aes(x = as.factor(X),
                          ymin = Mean - se,
                          ymax = Mean + se,
                          group = as.factor(Colour)),
                      width = 0.1,
                      colour = "black",
                      position = position_dodge(0.9))
    } else{

      SPlot <- SPlot +
        geom_point(data = Errordf,
                   aes(y = Mean),
                   colour = "black",
                   position = position_dodge(0.9)) +
        geom_errorbar(data = Errordf, inherit.aes = F,
                      aes(x = as.factor(X),
                          ymin = Mean - se,
                          ymax = Mean + se),
                      width = 0.1,
                      colour = "black",
                      position = position_dodge(0.9))

    }
  }

  if(z != y) SPlot <- SPlot + labs(colour = z)

  if(Order) SPlot <- SPlot + scale_x_discrete(limits = rev(unique(Errordf$X)))
  if(Just) SPlot <- SPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  SPlot

}
