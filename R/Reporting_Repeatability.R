#' Deriving Repeatability as a Function
#'
#' @param Model
#'
#' @return Currently, returns nothing.
#' @export
#'
#' @examples
RepeatabilityReport <- function(Model){
  # require(MCMCglmm)
  requireNamespace("MCMCglmm", quietly = TRUE)

  Beta0<-mean(Model$X%*%Model$Sol[i,1:ncol(Model$X)]) #Intercept

  Va<-Model$VCV[,"Name"] #Associated variance
  Ve<-Model$VCV[,"units"]+Model$VCV[,"Season:Name"]

  Expected<-exp(Beta0+(0.5*(Va+Ve)))

  Repeatability1<-(Expected*(exp(Va)-1))/(Expected*(exp(Va+Ve)-1)+1)
  print("Name")

  print(paste(round(posterior.mode(Repeatability1),digits=2)," ","(",round(HPDinterval(Repeatability1)[,1],digits=2),",",round(HPDinterval(Repeatability1)[,2],digits=2),")",sep=""))

  Va<-Model$VCV[,"Season:Name"] #Associated variance
  Ve<-Model$VCV[,"units"]+Model$VCV[,"Name"] #Error variance

  Expected<-exp(Beta0+0.5*(Va+Ve))

  Repeatability2<-(Expected*(exp(Va)-1))/(Expected*(exp(Va+Ve)-1)+1)
  print("Name:Season")
  print(paste(round(posterior.mode(Repeatability2),digits=2)," ","(",round(HPDinterval(Repeatability2)[,1],digits=2),",",round(HPDinterval(Repeatability2)[,2],digits=2),")",sep=""))

}
