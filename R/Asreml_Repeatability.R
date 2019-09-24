#' AsReml Repeatability
#'
#' @param model
#'
#' @return
#' @export
#'
#' @rdname ASR
#'
#' @examples
AsRepPlot<-function(model){
  require(asreml);require(ggplot2)
  V<-list()
  RepDf<-matrix(NA,nrow=nrow(summary(model,all=TRUE)$varcomp)-1,ncol=1)
  rownames(RepDf)<-rownames(summary(model,all=TRUE)$varcomp)[1:(nrow(summary(model,all=TRUE)$varcomp)-1)]
  print(summary(model,all=TRUE)$distribution)
  if(summary(model,all=TRUE)$distribution=="gaussian"){
    VTotal<-sum(summary(model,all=TRUE)$varcomp$gamma)
    for(x in 1:(length(summary(model,all=TRUE)$varcomp$gamma)-1)){
      V[x]<-summary(model,all=TRUE)$varcomp$gamma[x]
      RepDf[x,1]<-V[x]/VTotal
    }
  }
  if(summary(model,all=TRUE)$distribution=="binomial"){
    VTotal<-sum(summary(model,all=TRUE)$varcomp$gamma)
    for(x in 1:(length(summary(model,all=TRUE)$varcomp$gamma)-1)){
      RepDf[x,1]<-summary(model,all=TRUE)$varcomp$gamma[x]/VTotal
    }
  }
  print(RepDf)
}
