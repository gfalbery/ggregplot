INLADICFig <- function(ModelList, ModelNames = NULL, Just = F, CutOff = 2){
  require(INLA); require(ggplot2)

  if(!class(ModelList)=="list"){
    ModelList <- list(ModelList)
  }

  df <- data.frame(Model = 1:length(ModelList),
                   DIC = sapply(ModelList, function(m) m$dic$dic))

  if(is.null(ModelNames)){
    ModelNames <- 1:length(ModelList)
  }

  df$Competitive <- with(df, ifelse(DIC<min(DIC + CutOff), "Y", "N"))

  if(Just){ Angle = 45; Hjust = 1 }else{ Angle = 0; Hjust = 0.5}

  ggplot(df, aes(as.numeric(Model), DIC)) +
    geom_point(aes(shape = Competitive)) + scale_shape_manual(values = c(1,2)) +
    geom_line() +
    labs(x = "Model") +
    scale_x_continuous(breaks = as.numeric(df$Model),labels = ModelNames) +
    geom_point(data = df[df$Competitive == "Y",]) +
    theme(axis.text.x = element_text(angle = Angle, hjust = Hjust))

}

