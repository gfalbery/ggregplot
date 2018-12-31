# Liverpool era effect size comparison ####

MCMCFxComp<-function(model, vars, numeric = T){
  
  FactorCompList<-list()
  
  Solutions<-model$Sol[,1:dim(model$X)[2]]
  
  if(!is.null(vars)){
    for(x in 1:length(vars)){
      
      Factors <- vars[x]
      Columns<-Solutions[,substr(colnames(Solutions),1,nchar(Factors))==Factors]
      if(dim(Columns)[2]>1){
        Columns <- cbind(0, Columns)
        df1<-apply(Columns,2,function(b)
          apply(Columns,2,function(a) mean(a - b)[1]))
        
        df1l<-apply(Columns,2,function(b)
          apply(Columns,2,function(a) HPDinterval(as.mcmc(a - b))[1]))
        
        df1u<-apply(Columns,2,function(b)
          apply(Columns,2,function(a) HPDinterval(as.mcmc(a - b))[2]))
        
        df1p<-apply(Columns,2,function(b) 
          apply(Columns,2,function(a) (dim(Columns)[1]-max(table(a>b)))/(dim(Columns)[1]/2)))
        
        #df1[,1]<-summary(model)$solutions[rownames(summary(model)$solutions)%in%colnames(Columns),"post.mean"]
        
        dim(df1)<-rep(ncol(Columns),2)
        dimnames(df1) <- dimnames(df1l) <- dimnames(df1u) <- dimnames(df1p) <-list(substr(colnames(Columns),nchar(Factors)+1,100),substr(colnames(Columns),nchar(Factors)+1,100))
        colnames(df1)[1] <- colnames(df1l)[1] <- colnames(df1u)[1] <- colnames(df1p)[1] <- "Intercept" 
        rownames(df1)[1] <- rownames(df1l)[1] <- rownames(df1u)[1] <- rownames(df1p)[1] <- "Intercept" 
        FactorCompList[[x]] <- list(Mean=df1, Lower=df1l, Upper=df1u, pMCMC=df1p)
        names(FactorCompList)[x]<-Factors
        
      }else{
        Column<-colnames(Solutions)[substr(colnames(Solutions),1,nchar(Factors))==Factors]
        df1<-summary(model)$solutions[Column,1]
        df1l<-summary(model)$solutions[Column,2]
        df1u<-summary(model)$solutions[Column,3]
        df1p<-summary(model)$solutions[Column,"pMCMC"]
        
        FactorCompList[[x]]<-list(Mean=df1, Lower=df1l, Upper=df1u, pMCMC=df1p)
        names(FactorCompList)[x]<-Factors
      }
    }
    
    if(numeric == F){
      FactorCompList[[x]] <- paste0(round(df1,3), " (", round(df1l,3), ", ", round(df1u,3), ") ",
                                    cut(df1p,breaks=c(-0.1,0.001,0.01,0.05,1),labels=c("***","**","*",""))) 
      
      if(length(FactorCompList[[x]])>0){
        dim(FactorCompList[[x]])<-rep(ncol(Columns),2)
        dimnames(FactorCompList[[x]])<-list(substr(colnames(Columns),nchar(Factors)+1,100),substr(colnames(Columns),nchar(Factors)+1,100))
      }
    }
    
  } else {
    Factors <- unique(unlist(strsplit(substr(unlist(summary(model)$fixed.formula)[3],1,1000),split=" ")))
    Factors<-Factors[-which(Factors=="+")]
    
    Solutions<-model$Sol[,1:dim(model$X)[2]]
    for(x in 1:length(Factors)){
      Columns<-Solutions[,substr(colnames(Solutions),1,nchar(Factors))==Factors]
      if(dim(Columns)[2]>1){
        Columns <- cbind(0, Columns)
        
        df1<-apply(Columns,2,function(b)
          apply(Columns,2,function(a) mean(a - b)[1]))
        
        df1l<-apply(Columns,2,function(b)
          apply(Columns,2,function(a) HPDinterval(as.mcmc(a - b))[1]))
        
        df1u<-apply(Columns,2,function(b)
          apply(Columns,2,function(a) HPDinterval(as.mcmc(a - b))[2]))
        
        df1p<-apply(Columns,2,function(b) 
          apply(Columns,2,function(a) (dim(Columns)[1]-max(table(a>b)))/(dim(Columns)[1]/2)))
        
        
        dim(df1)<-rep(ncol(Columns),2)
        dimnames(df1)<-list(substr(colnames(Columns),nchar(Factors)+1,100),substr(colnames(Columns),nchar(Factors)+1,100))
        colnames(df1)[1] <- colnames(df1l)[1] <- colnames(df1u)[1] <- colnames(df1p)[1] <- "Intercept" 
        rownames(df1)[1] <- rownames(df1l)[1] <- rownames(df1u)[1] <- rownames(df1p)[1] <- "Intercept" 
        FactorCompList[[x]]<- list(Mean = df1, Lower = df1l, Upper = df1u, pMCMC = df1p)
        names(FactorCompList)[x]<-Factors
        
      }else{
        Column<-colnames(Solutions)[substr(colnames(Solutions),1,nchar(Factors))==Factors]
        df1<-summary(model)$solutions[Column,1]        
        df1l<-summary(model)$solutions[Column,2]
        df1u<-summary(model)$solutions[Column,3]
        df1p<-summary(model)$solutions[Column,"pMCMC"]
        
        FactorCompList[[x]]<-list(Mean = df1, Lower = df1l, Upper = df1u, pMCMC = df1p)
        names(FactorCompList)[x]<-Factors
      }
    }
    
    if(numeric == F){
      FactorCompList[[x]] <- paste0(round(df1,3), " (", round(df1l,3), ", ", round(df1u,3), ") ",
                                    cut(df1p,breaks=c(-0.1,0.001,0.01,0.05,1),labels=c("***","**","*",""))) 
      
      if(length(FactorCompList[[x]])>0){
        dim(FactorCompList[[x]])<-rep(ncol(Columns),2)
        dimnames(FactorCompList[[x]])<-list(substr(colnames(Columns),nchar(Factors)+1,100),substr(colnames(Columns),nchar(Factors)+1,100))
      }
    }
  }
  
  FactorCompList
  
}
