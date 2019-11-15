ModelPlote<-function(model){
  graph<-summary(model)$solutions
  ggplot(as.data.frame(graph),aes(x=rownames(graph),y=post.mean))+geom_point()+geom_errorbar(aes(ymin=graph[,"l-95% CI"],ymax=graph[,"u-95% CI"]),size=0.3,width=0.4)+
    geom_hline(aes(yintercept=0),lty=2)+THEME+labs(x=NULL)+
    geom_text(aes(y=max(graph[,"u-95% CI"])*1.1,label=cut(pMCMC,breaks=c(0,0.001,0.01,0.05,1),labels=c("***","**","*",""))))+coord_flip()
}
