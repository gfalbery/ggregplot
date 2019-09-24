#' ModelVarPlote
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
ModelVarPlote<-function(model){
  graph<-unique(data.frame(rbind(summary(model)$Gcovariances,summary(model)$Rcovariances)))
  graph$Factor<-0
  graph$Factor<-factor(rownames(graph),levels=rownames(graph))
  ggplot(as.data.frame(graph),aes(x=Factor,y=post.mean))+geom_point()+geom_errorbar(aes(ymin=graph[,"l.95..CI"],ymax=graph[,"u.95..CI"]),size=0.3,width=0.4)+
    geom_hline(aes(yintercept=0),lty=2)+THEME+labs(x=NULL)+coord_flip()
}

