INLADICFig <- function(ModelList, ModelNames = NULL){
  require(INLA); require(ggplot2)
  df <- data.frame(Model = 1:length(ModelList),
                   DIC = sapply(ModelList, function(m) m$dic$dic))

  if(is.null(ModelNames)){
    ModelNames <- 1:length(ModelList)
  }

  df$Competitive <- with(df, ifelse(DIC<min(DIC + 2), "Y", "N"))

  ggplot(df, aes(as.numeric(Model), DIC)) + geom_point() + geom_line() + THEME +
    scale_x_continuous(breaks = as.numeric(df$Model),labels = ModelNames) + geom_point(data = df[df$Competitive == "Y",])

}
