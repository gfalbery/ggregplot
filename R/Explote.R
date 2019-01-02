Explote<-function(x,df,Aesthetic="variable",Method="lm",Parasites=MainParasites){
  require(reshape2);require(ggplot2);require(grid)
  RDeer_long<-melt(df,
                   id.vars=colnames(df)[!colnames(df)%in%Parasites],
                   value.name="EPG")
  RDeer_long$EPG<-RDeer_long$EPG/RDeer_long$cFDM
  X<-RDeer_long[,x]
  data<-RDeer_long
  ParasiteColoursUse<-ParasiteColours[1:length(Parasites)]
  AESTHETIC<-data[,Aesthetic]
  if(is.numeric(X)){
    ggplot(data,aes(X,log(EPG+1),colour=variable,shape=AESTHETIC))+
      facet_wrap(~variable)+
      geom_point()+
      geom_smooth(method=Method,aes(lty=AESTHETIC))+
      THEME+theme(strip.background = element_rect(fill = "white", color = "grey", size = 0))+
      scale_color_manual(values=ParasiteColoursUse)+
      xlab(x)
  }else{
    if(is.factor(X)|is.character(X)){
      ggplot(data,aes(X,log(EPG+1),colour=variable,shape=AESTHETIC))+
        facet_wrap(~variable)+
        geom_violin(aes(lty=AESTHETIC))+
        geom_point(position=position_dodge(w=0.9))+
        THEME+theme(strip.background = element_rect(fill = "white", color = "grey", size = 0))+
        scale_color_manual(values=ParasiteColoursUse)+
        xlab(x)+
        stat_summary(fun.data=mean_se, fun.args = list(mult=1),geom="errorbar",width=0.35,colour="black",position=position_dodge(w=0.9))+
        stat_summary(fun.y = "mean", geom = "point",size= 1.5,colour="black",shape=3,position=position_dodge(w=0.9))
    }
  }
}
