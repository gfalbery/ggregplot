# EfxPlotComp

Efxplot <- function(ModelList, Sig = TRUE,
                    ModelNames = NULL,
                    VarNames = NULL, VarOrder = NULL,
                    Intercept = TRUE,
                    tips = 0.2){

  require(dplyr); require(ggplot2); require(INLA); require(MCMCglmm)

  graphlist<-list()

  if(!class(ModelList)=="list"){
    ModelList <- list(ModelList)
  }

  for(i in 1:length(ModelList)){
    model<-ModelList[[i]]

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

  graph$Sig <- with(graph,ifelse((Lower<0&Upper<0)|(Lower>0&Upper>0),"*",""))

  graph$Model <- as.factor(graph$Model)

  if(!is.null(ModelNames)){
    levels(graph$Model)<-ModelNames
  }

  position <- ifelse(length(unique(graph$Model)) == 1, "none", "right")

  if(is.null(VarOrder)) VarOrder <- rev(unique(graph$Factor))
  if(is.null(VarNames)) VarNames <- VarOrder

  graph$Factor <- factor(graph$Factor, levels = VarOrder)
  levels(graph$Factor) <- VarNames

  if(!Intercept){

    VarNames <- VarNames[!str_detect(VarNames, "ntercept")]

    graph <- graph %>% filter(Factor %in% VarNames)

  }

  graph$starloc<-NA

  min<-min(graph$Lower,na.rm=T)
  max<-max(graph$Upper,na.rm=T)

  if(Sig==TRUE){

    graph$starloc <- max+(max-min)/10

  }

  ggplot(as.data.frame(graph),aes(x=as.factor(Factor),y=Estimate,colour=Model))+
    geom_point(position=position_dodge(w=0.5))+
    geom_errorbar(position=position_dodge(w=0.5), aes(ymin = Lower, ymax = Upper), size=0.3, width=tips)+
    geom_hline(aes(yintercept=0),lty=2) + labs(x=NULL) + coord_flip() +
    theme(legend.position = position) +
    geom_text(aes(label = Sig, y = starloc), position = position_dodge(w = 0.5), show.legend = F)

}



