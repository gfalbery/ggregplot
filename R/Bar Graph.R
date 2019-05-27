
BarGraph <- function(df, x, y, z = x,
                     Order = F,
                     Just = F,
                     Text = F){

  require(ggplot2); require(dplyr); require(ggforce)

  df <- df[,c(x,y,z)] %>% na.omit()

  df$X <- as.factor(df[,x])
  df$Y <- df[,y]
  df$Colour <- as.factor(df[,z])

  Errordf <- df %>% group_by(X, Colour) %>%
    summarise(Mean = mean(Y, na.rm = T),
              sd = sd(Y, na.rm = T),
              N = n()) %>%
    mutate(se = sd/(N^0.5)) %>%
    as.data.frame() %>%
    slice(order(Mean, decreasing = T)) %>% na.omit()

  if(Order) Errordf <- Errordf %>% slice(order(Mean)) %>% mutate(X = factor(X, levels = unique(X)), Colour = factor(Colour, levels = unique(Colour)))

  SPlot <- df %>%
    ggplot(aes(X, Y, fill = Colour)) +
    labs(x = x, y = y) +
    geom_col(data = Errordf,
             aes(y = Mean, group = Colour),
             colour = "black",
             position = position_dodge(0.9)) +

    geom_errorbar(data = Errordf, inherit.aes = F,
                  aes(x = X,
                      ymin = Mean - se,
                      ymax = Mean + se,
                      group = Colour),
                  width = 0.1,
                  colour = "black",
                  position = position_dodge(0.9))

  if(z != y) SPlot <- SPlot + labs(colour = z)

  if(Just) SPlot <- SPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if(Text == "N"){

    PositionT <- (max(abs(c(Errordf$Mean + Errordf$se,Errordf$Mean - Errordf$se)), na.rm = T))/15

    SPlot <- SPlot + geom_text(data = Errordf, aes(y = ifelse(Mean + se > 0,
                                                              Mean + se + PositionT,
                                                              PositionT),label = N),
                               position=position_dodge(.9))

  }

  SPlot

}
