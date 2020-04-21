
# Meta-Analysis Model Addition Code ####


MetaforAdd <- function(Response, Errors,
                       Explanatory, 
                       Add, Random = NULL,
                       Rounds = Inf,
                       Clashes = NULL,
                       AllModels = F,
                       RandomModel = NULL, 
                       Family = "gaussian", Data, Delta = 2,
                       OptMethod = "BFGS",
                       Optimiser = "optim"){
  
  require(INLA); require(ggplot2)
  
  Explanatory2 <- paste(Explanatory, collapse = " + ")
  
  Random2 <- paste(paste0("f(",Random, ", model = '", RandomModel, "')"), collapse = " + ")
  f1 <- as.formula(paste0(Response, " ~ ", paste(Explanatory2, " + ", Random2, collapse = " + ")))
  
  BaseModel <-  rma.mv(yi = yi, V = vi,
                       random = list(~1|StudySystem/Observation),
                       method = "ML", 
                       mods = f1, 
                       data = Data,
                       control = list(optimizer = Optimiser, optmethod = OptMethod))
  
  
  ModelList <- AllModelList <- RemovedList <- FullFormulaList <- FormulaList <- list()
  AICList <- dAICList <- list()
  
  AICList[["Base"]] <- BaseModel$AIC$AIC
  FullFormulaList[["Base"]] <- f1
  
  for(x in 1:length(Add)){
    
    "Adding: " %>% paste0(Add[x]) %>% print
    
    Explanatory3 <- paste(c(Explanatory, Add[x]), collapse = " + ")
    
    f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))
    
    Model1 <- rma.mv(yi = yi, V = vi,
                     random = list(~1|StudySystem/Observation),
                     method = "ML", 
                     mods = f2, 
                     data = Data,
                     control = list(optimizer = Optimiser, optmethod = OptMethod))
    
    ModelList[[Add[x]]] <- Model1
    
    FormulaList[[Add[x]]] <- f2
    
  }
  
  AICList[[2]] <- sapply(ModelList, function(y) y$AIC$AIC)
  names(AICList[[2]]) <- Add
  dAICList[[1]] <- AICList[[2]] - AICList[[1]]
  names(dAICList[[1]]) <- Add
  RemovedList[[1]] <- Add
  
  FullFormulaList[[2]] <- FormulaList
  
  AllModelList[[1]] <- BaseModel
  AllModelList[[2]] <- ModelList
  
  NewExplanatory <- Explanatory
  
  Add2 <- Add
  
  if((min(dAICList[[length(dAICList)]]) < -Delta)&(Rounds>1)&(length(Add2)>0)){
    
    while((min(dAICList[[length(dAICList)]]) < -Delta)&(Rounds>1)&(length(Add2)>0)){
      
      Rounds <- Rounds - 1
      
      Kept <- Add2[which(dAICList[[length(dAICList)]] == min(dAICList[[length(dAICList)]]))]
      
      ModelList <- FormulaList <- list()
      
      NewExplanatory <- c(NewExplanatory, Add2[which(dAICList[[length(dAICList)]] == min(dAICList[[length(dAICList)]]))])
      
      Add2 <- Add2[-which(dAICList[[length(dAICList)]] == min(dAICList[[length(dAICList)]]))]
      
      if(length(Add2)>0){
        
        print(paste("Keeping", Kept))
        
        print(Text <- paste("Run", length(AICList)))
        
        if(Kept %in% unlist(Clashes)){
          
          ClashRemove <- Clashes[Clashes %>% map_lgl(~Kept %in% .x)] %>% unlist
          
          Add2 <- Add2 %>% setdiff(ClashRemove)
          
          "Removing clashes: " %>% paste0(paste0(ClashRemove, collapse = "; ")) %>% print
          
        }
        
        if(length(Add2)>0){
          
          for(x in 1:length(Add2)){
            
            "Adding: " %>% paste0(Add2[x]) %>% print
            
            Explanatory3 <- paste(c(NewExplanatory, Add2[x]), collapse = " + ")
            
            f2 <- as.formula(paste0(Response, " ~ ", paste(Explanatory3, collapse = " + ")))
            
            Model1 <- rma.mv(yi = yi, V = vi,
                             random = list(~1|StudySystem/Observation),
                             method = "ML", 
                             mods = f2, 
                             data = Data,
                             control = list(optimizer = Optimiser, optmethod = OptMethod))
            
            ModelList[[Add2[x]]] <- Model1
            
            FormulaList[[Add2[x]]] <- f2
            
          }
          
          AllModelList[[length(AllModelList) + 1]] <- ModelList
          FullFormulaList[[length(FullFormulaList) + 1]] <- FormulaList
          
          AICList[[length(AICList) + 1]] <- sapply(ModelList, AIC)
          names(AICList[[length(AICList)]]) <- Add2
          dAICList[[length(dAICList) + 1]] <- AICList[[length(AICList)]] - min(AICList[[length(AICList)-1]])
          names(dAICList[[length(dAICList)]]) <- Add2
          
          RemovedList[[length(RemovedList) + 1]] <- Add2
          
        }
      }
    }
    
    if(length(dAICList)>0){
      
      if(all(last(dAICList)< -Delta)){
        
        FinalModel <- AllModelList[[length(AllModelList)]][[1]]
        FinalFormula <- FullFormulaList[[length(AllModelList)]][[1]]
        
        print(paste("Keeping Everything!"))
        
      }else{
        
        FinalModel <- AllModelList[[length(AllModelList)-1]][[which(dAICList[[length(dAICList)-1]] == min(dAICList[[length(dAICList)-1]]))]]
        FinalFormula <- FullFormulaList[[length(AllModelList)-1]][[which(dAICList[[length(dAICList)-1]] == min(dAICList[[length(dAICList)-1]]))]]
        
        print(paste("Not Keeping ", paste(Add2, collapse = " ")))
        
      }
      
    }else{
      
      FinalModel <- BaseModel
      FinalFormula <- f1
      
      print("Nothing Kept")
      
    }
    
  }else{
    
    FinalModel <- BaseModel
    FinalFormula <- f1
    
    print("Nothing Kept")
    
  }
  
  ReturnList <- list(FinalModel = FinalModel,
                     Removed = RemovedList,
                     AIC = AICList,
                     dAIC = dAICList,
                     FormulaList = FullFormulaList,
                     FinalFormula = FinalFormula)
  
  if(AllModels){
    
    ReturnList$AllModels <- AllModelList
    
  }
  
  return(ReturnList)
  
}





