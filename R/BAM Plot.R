
# BAM Plot ####

Model <- BAM1$FinalModel

Data <- BAM1$Data

PlotVariable <- "Density.Annual"

BAMPlot <- function(
  
  Model, Data, PlotVariable,
  
  Resolution = 100,
  
  PlotMethod = "Default",
  
  TrimData = T,
  SEFit = T
  
){
  
  Covar <- Model$terms[[3]] %>% as.character %>% extract(2:length(.))
  
  Response <- Model$terms[[2]] %>% as.character
  
  Data$Y <- Data[,Response]
  
  Predict <- predict(Model, newdata = Data)
  
  Intercept <- Predict %>% mean
  
  PlotList <- list()
  
  SeqList <- 
    Covar %>% seq_along %>% 
    
    map(function(r){
      
      if(class(Data[,Covar[r]]) %in% c("integer", "numeric")){
        
        seq(from = min(Data[,Covar[r]]),
            to = max(Data[,Covar[r]]),
            length.out = Resolution) %>% 
          return
        
      }else{
        
        Data[,Covar[r]] %>% unique %>% sort %>% return
        
      }
      
    })
  
  NewData <- expand.grid(SeqList)
  
  colnames(NewData) <- Covar
  
  Medians <- SeqList %>% map(~.x[round(length(.x)/2)])
  
  PlotData <- NewData
  
  if(TrimData){
    
    for(r in seq_along(Medians)){
      
      if(!Covar[r] %in% PlotVariable){
        
        PlotData <- PlotData[PlotData[,Covar[r]] == Medians[[r]],]
        
      }
    }
  }
  
  PlotData
  
  FitDF <-
    predict(Model, PlotData, se.fit = SEFit) %>% 
    bind_cols %>% rename(Fit = fit, SE = se.fit) %>% 
    bind_cols(PlotData, .)
  
  for(r in seq_along(PlotVariable)){
    
    FitDF$X <- FitDF[,PlotVariable[r]]
    Data$X <- Data[,PlotVariable[r]]
    
    BasePlot <- 
      FitDF %>% ggplot(aes(X, Fit))
    
    if(PlotMethod == "Default"){
      
      BasePlot <- 
        BasePlot + 
        geom_point(data = Data, 
                   aes(X, Y),
                   colour = AlberColours[[1]], alpha = 0.3) +
        geom_ribbon(aes(ymin = Fit - SE,
                        ymax = Fit + SE),
                    alpha = 0.3) +
        geom_line()
      
    }
    
    PlotList[[PlotVariable[r]]] <- BasePlot
    
  }
  
  PlotList %>% return
  
}

# Response <- Model$terms[[2]]
# 
# Covar <- Model$terms[[3]] %>% as.character %>% extract(2:length(.))
# 
# Formula <- 
#   paste0(Response, " ~ ", paste(Covar, collapse = " + ")) %>% 
#   as.formula
# 
# MM <-
#   model.matrix(Formula, data = Data)
# 
# Model$coefficients %>% length