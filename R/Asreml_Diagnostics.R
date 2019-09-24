########## asreml diagnostics

AsDiagnostics<-function(model){
  par(ask=F,mfrow=c(1,2))
  plot(AsYoung$fitted.values,AsYoung$residuals)
  plot(AsYoung$residuals[order(AsYoung$residuals)])
}
