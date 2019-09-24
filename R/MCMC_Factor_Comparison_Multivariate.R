########### MCMC Factor Comparison for Multivariate Models

MCMCFactorComp2<-function(Model,Response,Factor){
  Solutions<-Model$Sol[,1:dim(Model$X)[2]]
  COLNAME<-paste("trait",Response,":",Factor,sep="")
  Columns<-Solutions[,substr(colnames(Solutions),1,nchar(COLNAME))==COLNAME]    
  df1<-matrix(NA,nrow=ncol(Columns),ncol=ncol(Columns))
  if(dim(df1)[1]>0){
    df1<-apply(Columns,2,function(b) 
      apply(Columns,2,function(a) (1000-max(table(a>b)))/500))
    diag(df1)<-NA
    df1[lower.tri(df1)]<-NA
    LENGTH<-dim(df1)[2]+1
    df1[,1]<-summary(Model)$solutions[substr(colnames(Solutions),1,nchar(COLNAME))==COLNAME,"pMCMC"]
    
    df1<-matrix(paste(df1,cut(df1,breaks=c(-0.1,0.001,0.01,0.05,1),labels=c("***","**","*","")),sep=""),ncol=ncol(Columns),nrow=ncol(Columns))
    df1<-factor(df1,levels=c(unique(c(df1)),"<0.001***",""))
    df1[df1=="0***"]<-"<0.001***"
    df1[df1=="NANA"]<-""
    
    dim(df1)<-rep(ncol(Columns),2)
    dimnames(df1)<-list(colnames(Columns),colnames(Columns))
    colnames(df1)[1]<-"Intercept"
    
    print(df1,max.levels=0)
    
  }
  }
