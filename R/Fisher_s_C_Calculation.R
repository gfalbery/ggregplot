#' Fisher's C calculation for Path Analysis Models
#'
#' @param df
#' @param daglist
#'
#' @return
#' @export
#'
#' @examples
FisherC<-function(df,daglist){
  for(d in 1:length(daglist)){
    graphlist[[d]]<-melt(daglist[[d]])
    graphlist[[d]]<-subset(graphlist[[d]],value==1)
    colnames(graphlist[[d]])<-c("from","to","weight")
    links<-graphlist[[d]]
    nodes<-unique(c(as.character(graphlist[[d]]$from),as.character(graphlist[[d]]$to)))
    netlist[[d]] <- graph.data.frame(links, nodes, directed=TRUE)

    E(netlist[[d]])$color<-"black"
    E(netlist[[d]])$arrow.size<-0.5
    E(netlist[[d]])$width<-2
    V(netlist[[d]])$frame.color <- NA
    V(netlist[[d]])$color<-"white"
    V(netlist[[d]])$size<-30
    V(netlist[[d]])$label.cex<-1
    V(netlist[[d]])$label.color<-"black"

    Set<-basiSet(daglist[[d]])
    #Set<-Set[!sapply(Set,function(l) l[[1]]%in%c("Year","Season"))]

    ModelList[[d]]<-ModelList2<-list()

    if(length(Set)>0){
      for(e in 1:length(Set)){
        df$Response<-df[,Set[[e]][1]]
        df$Explanatory<-df[,Set[[e]][2]]
        #if(nunique(df$Explanatory)==2){df$Explanatory<-as.factor(df$Explanatory)}

        vars<-Set[[e]][3:length(Set[[e]])]

        if(length(Set[[e]])>2){
          df[,paste0("X",(3:length(Set[[e]])-2))]<-df[,Set[[e]][3:length(Set[[e]])]]
        }

        if(!is.factor(df$Response)&!(nunique(df$Response)==2&is.numeric(df$Response))){
          if(length(Set[[e]])==2){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory,random=~Name)
          }
          if(length(Set[[e]])==3){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1,random=~Name)
          }
          if(length(Set[[e]])==4){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2,random=~Name)
          }
          if(length(Set[[e]])==5){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2+X3)
          }
          if(length(Set[[e]])==6){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2+X3+X4)
          }
          if(length(Set[[e]])==7){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2+X3+X4+X5)
          }
          if(length(Set[[e]])==8){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2+X3+X4+X5+X6)
          }
        }
        if(nunique(df$Response)==2&is.numeric(df$Response)){
          if(length(Set[[e]])==2){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory,random=~Name,family=asreml.binomial())
          }
          if(length(Set[[e]])==3){
            df$X1<-df[,Set[[e]][3]]
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1,random=~Name,family=asreml.binomial())
          }
          if(length(Set[[e]])==4){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2,random=~Name,family=asreml.binomial())
          }
          if(length(Set[[e]])==5){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2+X3,family=asreml.binomial())
          }
          if(length(Set[[e]])==6){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2+X3+X4,family=asreml.binomial())
          }
          if(length(Set[[e]])==7){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2+X3+X4+X5,family=asreml.binomial())
          }
          if(length(Set[[e]])==8){
            ModelList2[[e]]<-asreml(data=df,Response~Explanatory+X1+X2+X3+X4+X5+X6,family=asreml.binomial())
          }
        }
      }
    }
    CList[[d]]<-0
    if(length(ModelList2)>0){
      Chisqs<-sapply(ModelList2,function(a) (data.frame(anova(a))["Explanatory","Pr.Chisq."]))
      Chisqs[Chisqs==0]<-0.001
      CList[[d]]<-(-2*sum(log(Chisqs)))
    }
    ModelList[[d]]<-ModelList2
  }
}
