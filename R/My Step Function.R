MyStep<-function(model,gap=4){
  aic<-AIC(model)
  model2<-as.data.frame(drop1(model))
  while(max(model2$AIC)>(aic+gap)){
    print(rownames(model2)[which(model2$AIC==max(model2$AIC))])
    print(max(model2$AIC))
    model2<-model2[-which(model2$AIC==max(model2$AIC)),]
  }
  return(model2)
}

MyStep<-function(model,gap=4,override=Inf){
  aic<-AIC(model)
  model2<-as.data.frame(drop1(model))
  I<-0
  while(max(model2$AIC)>(aic+gap)&I<override){
    print(rownames(model2)[which(model2$AIC==max(model2$AIC))])
    print(max(model2$AIC))
    model2<-model2[-which(model2$AIC==max(model2$AIC)),]
  }
  return(model2)
  I<-I+1
}

MyStep<-function(model,gap=4,override=Inf){
  aic<-model$dic$dic
  model2<-as.data.frame(drop1(model))
  I<-0
  while(max(model2$AIC)>(aic+gap)&I<override){
    print(rownames(model2)[which(model2$AIC==max(model2$AIC))])
    print(max(model2$AIC))
    model2<-model2[-which(model2$AIC==max(model2$AIC)),]
  }
  return(model2)
  I<-I+1
}
  