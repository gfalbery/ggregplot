# Liverpool era factor comparison

MCMCFactorComp <- function(model, vars){

  require(MCMCglmm)

  FactorCompList<-list()

  Solutions<-model$Sol[,1:dim(model$X)[2]]

  if(!is.null(vars)){
    for(x in 1:length(vars)){

      Factors <- vars[x]
      Columns<-Solutions[,substr(colnames(Solutions),1,nchar(Factors))==Factors]
      if(!is.null(dim(Columns))){
        df1<-matrix(NA,nrow=ncol(Columns),ncol=ncol(Columns))
        colnames(df1) <- rownames(df1) <- substr(colnames(Columns), nchar(Factors)+1, sapply(colnames(Columns), nchar))
        if(dim(df1)[1]>0){
          df1<-apply(Columns,2,function(b)
            apply(Columns,2,function(a) (dim(Columns)[1]-max(table(a>b)))/(dim(Columns)[1]/2)))
          diag(df1)<-NA
          df1[lower.tri(df1)]<-NA
          df1[,1]<-summary(model)$solutions[rownames(summary(model)$solutions)%in%colnames(Columns),"pMCMC"]

          df1<-matrix(paste(df1,cut(df1,breaks=c(-0.1,0.001,0.01,0.05,1),labels=c("***","**","*","")),sep=""),ncol=ncol(Columns),nrow=ncol(Columns))
          df1<-factor(df1,levels=c(unique(c(df1)),"<0.001***",""))
          df1[df1=="0***"]<-"<0.001***"
          df1[df1=="NANA"]<-""

          dim(df1)<-rep(ncol(Columns),2)
          dimnames(df1)<-list(substr(colnames(Columns),nchar(Factors)+1,100),substr(colnames(Columns),nchar(Factors)+1,100))
          colnames(df1)[1]<-"Intercept"
          FactorCompList[[x]]<-df1
          names(FactorCompList)[x]<-Factors
        }
      }else{
        Column<-colnames(Solutions)[substr(colnames(Solutions),1,nchar(Factors))==Factors]
        df1<-round(summary(model)$solutions[Column,"pMCMC"],3)
        df1<-as.character(paste(df1,cut(df1,breaks=c(-0.1,0.001,0.01,0.05,1),labels=c("***","**","*","")),sep=""))
        df1[df1=="0***"]<-"<0.001***"
        df1<-as.factor(df1)
        FactorCompList[[x]]<-df1
        names(FactorCompList)[x]<-Factors
      }
    }

  } else {
    Factors <- unique(unlist(strsplit(substr(unlist(summary(model)$fixed.formula)[3],1,1000),split=" ")))
    Factors<-Factors[-which(Factors=="+")]

    Solutions<-model$Sol[,1:dim(model$X)[2]]
    for(x in 1:length(Factors)){
      Columns<-Solutions[,substr(colnames(Solutions),1,nchar(Factors))==Factors]
      if(!is.null(dim(Columns))){
        df1<-matrix(NA,nrow=ncol(Columns),ncol=ncol(Columns))
        if(dim(df1)[1]>0){
          df1<-apply(Columns,2,function(b)
            apply(Columns,2,function(a) (dim(Columns)[1]-max(table(a>b)))/(dim(Columns)[1]/2)))
          diag(df1)<-NA
          df1[lower.tri(df1)]<-NA
          df1[,1]<-summary(model)$solutions[rownames(summary(model)$solutions)%in%colnames(Columns),"pMCMC"]

          df1<-matrix(paste(df1,cut(df1,breaks=c(-0.1,0.001,0.01,0.05,1),labels=c("***","**","*","")),sep=""),ncol=ncol(Columns),nrow=ncol(Columns))
          df1<-factor(df1,levels=c(unique(c(df1)),"<0.001***",""))
          df1[df1=="0***"]<-"<0.001***"
          df1[df1=="NANA"]<-""

          dim(df1)<-rep(ncol(Columns),2)
          dimnames(df1)<-list(substr(colnames(Columns),nchar(Factors)+1,100),substr(colnames(Columns),nchar(Factors)+1,100))
          colnames(df1)[1]<-"Intercept"
          FactorCompList[[x]]<-df1
          names(FactorCompList)[x]<-Factors
        }
      }else{
        Column<-colnames(Solutions)[substr(colnames(Solutions),1,nchar(Factors))==Factors]
        df1<-round(summary(model)$solutions[Column,"pMCMC"],3)
        df1<-as.character(paste(df1,cut(df1,breaks=c(-0.1,0.001,0.01,0.05,1),labels=c("***","**","*","")),sep=""))
        df1[df1=="0***"]<-"<0.001***"
        df1<-as.factor(df1)
        FactorCompList[[x]]<-df1
        names(FactorCompList)[x]<-Factors
      }
    }
  }
  FactorCompList
}
