# EfxPlotComp
library(ggplot2)
THEME<-theme(axis.text.x=element_text(size=12,colour="black"),axis.text.y=element_text(size=12,colour="black"))+theme(axis.title.x=element_text(vjust=-0.35),axis.title.y=element_text(vjust=1.2))+theme_bw()

Efxplot<-function(modellist, sig = TRUE, ModelNames = NULL, tips = 0.2){
  require(dplyr); require(ggplot2); require(INLA); require(asreml); require(MCMCglmm)
  graphlist<-list()
  for(i in 1:length(modellist)){
    model<-modellist[[i]]

    if(class(model)=="asreml"){
      graph<-as.data.frame(summary(model,all=TRUE)$coef.fixed)
      graph<-subset(graph,solution!=0)
      colnames(graph)[1]<-"Estimate"
      graph[,c("Lower","Upper")]<-cbind(graph[,"Estimate"]-graph[,"std error"],graph[,"Estimate"]+graph[,"std error"])
    }

    if(class(model)=="inla"){
      graph<-as.data.frame(summary(model)$fixed)
      colnames(graph)[which(colnames(graph)%in%c("0.025quant","0.975quant"))]<-c("Lower","Upper")
      colnames(graph)[which(colnames(graph)%in%c("0.05quant","0.95quant"))]<-c("Lower","Upper")
      colnames(graph)[which(colnames(graph)%in%c("mean"))]<-c("Estimate")
    }

    if(class(model)=="MCMCglmm"){
      graph<-as.data.frame(summary(model)$solutions)
      colnames(graph)[1:3]<-c("Estimate","Lower","Upper")
    }

    graph$Model<-i
    graph$Factor<-rownames(graph)

    graphlist[[i]]<-graph

  }

  graph<-bind_rows(graphlist)

  graph$starloc<-NA

  min<-min(graph$Lower,na.rm=T)
  max<-max(graph$Upper,na.rm=T)

  if(sig==TRUE){
    graph$starloc <- max+(max-min)/10
  }

  graph$Sig<-with(graph,ifelse((Lower<0&Upper<0)|(Lower>0&Upper>0),"*",""))

  graph$Model<-as.factor(graph$Model)

  if(!is.null(ModelNames)){
    levels(graph$Model)<-ModelNames
  }

  position <- ifelse(length(unique(graph$Model)) == 1, "none", "right")

  ggplot(as.data.frame(graph),aes(x=Factor,y=Estimate,colour=Model))+
    geom_point(position=position_dodge(w=0.5))+
    geom_errorbar(position=position_dodge(w=0.5), aes(ymin = Lower, ymax = Upper),size=0.3,width=tips)+
    geom_hline(aes(yintercept=0),lty=2) + THEME + labs(x=NULL) + coord_flip() +
    theme(legend.position = position) +
    geom_text(aes(label = Sig, y = starloc), position = position_dodge(w = 0.5))

}



