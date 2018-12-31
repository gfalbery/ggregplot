############ Bar Graph

require(ggplot2)

THEME <- theme(axis.text.x = element_text(size = 12, colour = "black"),
               axis.text.y = element_text(size = 12, colour = "black")) +
  theme(axis.title.x = element_text(vjust = -0.35),
        axis.title.y = element_text(vjust=1.2)) +
  theme_bw()

Prev<-function(y){ round((length(y[y>0])/length(y))*100)}

data_summary<-function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      N=length(x[[col]]),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }

  data_sum<-plyr::ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

BarGraph<-function(df,x,y,z=x,geom="Bar",text=FALSE,labels=NA){
  require(ggplot2);require(reshape2)
  df2<-data_summary(droplevels(na.omit(df[,c(x,y,z)])),varname=y,groupnames=unique(c(x,z)))
  if(!z==x){
    df2$Prevalence<-na.omit(melt(tapply(df[,y],list(df[,z],df[,x]),Prev)))$value
  }else{
    df2$Prevalence<-na.omit(melt(tapply(df[,y],df[,x],Prev)))$value
  }

  df2$Text<-df2[,text]

  PositionT<-(range(c(df2[,y]-df2$se,df2[,y]+df2$se),na.rm=T)[2]-range(c(df2[,y]-df2$se,df2[,y]+df2$se),na.rm=T)[1])/15

  if(geom=="Bar"){
    if(text%in%c("Prevalence","N")){
      ggplot(df2,aes(df2[,x],df2[,y],fill=as.factor(df2[,z])))+
        geom_bar(stat="identity",colour="black",position=position_dodge(.9))+
        geom_errorbar(aes(ymin=df2[,y]-se,ymax=df2[,y]+se),width=0.2,position=position_dodge(.9))+
        THEME+xlab(x)+ylab(y)+labs(fill=z)+
        geom_text(aes(y=ifelse(df2[,y]+se>0,df2[,y]+se+PositionT,PositionT),label=Text),position=position_dodge(.9))+
        geom_text(aes(label=labels,y=ifelse(df2[,y]+se>0,df2[,y]+se+PositionT*1.5,PositionT*1.5)),position=position_dodge(.9))
    }else{
      ggplot(df2,aes(df2[,x],df2[,y],fill=as.factor(df2[,z])))+
        geom_bar(stat="identity",colour="black",position=position_dodge(.9))+
        geom_errorbar(aes(ymin=df2[,y]-se,ymax=df2[,y]+se),width=0.2,position=position_dodge(.9))+
        THEME+xlab(x)+ylab(y)+labs(fill=z)+
        geom_text(aes(label=labels,y=ifelse(df2[,y]+se>0,df2[,y]+se+PositionT,PositionT)),position=position_dodge(.9))
    }
  }else{
    ggplot(df2,aes(df2[,x],df2[,y],colour=as.factor(df2[,z])))+
      geom_point(position=position_dodge(.9),size=5)+
      geom_errorbar(aes(ymin=df2[,y]-se,ymax=df2[,y]+se),width=0.2,position=position_dodge(.9))+
      THEME+xlab(x)+ylab(y)+labs(fill=z)+
      geom_text(aes(label=labels,y=ifelse(df2[,y]+se>0,df2[,y]+se+PositionT*1.5,PositionT*1.5)),position=position_dodge(.9))
  }
}
