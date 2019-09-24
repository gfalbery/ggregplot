### #AsReml Plot

ASRPlot<-function(model){
  df<-na.omit(data.frame(summary(model,all=TRUE)$coef.fixed))
  df$Factor<-rownames(df)
  ggplot(df,aes(Factor,solution))+THEME+geom_point()+geom_errorbar(aes(ymin=solution-std.error,ymax=solution+std.error))+
    geom_hline(yintercept=0)
}
