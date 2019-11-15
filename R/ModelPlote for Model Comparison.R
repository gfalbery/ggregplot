######## ModelPlote for Model Comparison

ModelPloteComp<-function(ModelList,MultiModel=NULL,MultiVar=NULL,ModelNames=NULL,Angle=0){
  graph<-as.data.frame(summary(ModelList[[1]])$solutions)
  graph$Model<-1
  graph$Factor<-as.character(rownames(graph))
  if(length(ModelList)>1){
    for(i in 2:length(ModelList)){
      graph2<-as.data.frame(summary(ModelList[[i]])$solutions)
      graph2$Model<-i
      graph2$Factor<-as.character(rownames(graph2))
      graph<-rbind(graph,graph2)
    }
  }
  graph$Model<-as.factor(graph$Model)
  if(!is.null(ModelNames)){levels(graph$Model)<-ModelNames}
  
  if(!is.null(MultiVar)){
    graph3<-as.data.frame(summary(MultiModel)$solutions)
    charlength<-nchar(paste0("trait",MultiVar,":"))
    graph3<-graph3[substr(rownames(graph3),1,charlength)==paste0("trait",MultiVar,":")|rownames(graph3)==paste0("trait",MultiVar),]
    graph3$Model<-"Multivariate"
    graph3$Factor<-substr(rownames(graph3),(charlength+1),100)
    graph3[graph3$Factor=="","Factor"]<-"(Intercept)"
    
    graph<-rbind(graph,graph3)
  }
  
  ggplot(as.data.frame(graph),aes(x=Factor,y=post.mean,colour=Model))+
    geom_hline(aes(yintercept=0),lty=2)+THEME+labs(x=NULL)+
    geom_point(position=position_dodge(w=0.5),size=3)+geom_errorbar(aes(ymin=graph[,"l-95% CI"],ymax=graph[,"u-95% CI"]),size=0.3,width=0.4,position=position_dodge(w=0.5))+
    geom_text(angle=Angle,aes(y=max(graph[,"u-95% CI"])*1.1,label=cut(pMCMC,breaks=c(0,0.001,0.01,0.05,1),labels=c("***","**","*",""))),position=position_dodge(w=0.5))+coord_flip()
  
}
