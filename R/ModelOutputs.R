

ModelOutputs <- function(ModelList,
                         Sig = TRUE, StarLoc = NULL,
                         Alpha1 = 1, Alpha2 = 1,
                         PointOutline = T,
                         ModelNames = NULL,
                         Reverse = T, # If you reverse it you may need to then modify the labels
                         VarNames = NULL, VarOrder = NULL,
                         Intercept = TRUE, PointSize = 1,
                         BarWidth = 0.1,
                         tips = 0.2){
  
  require(dplyr); require(ggplot2); require(INLA); require(MCMCglmm); require(MASS)
  require(magrittr)
  
  Graphlist <- list()
  
  if(!any(class(ModelList) == "list")){
    
    ModelList <- list(ModelList)
    
  }
  
  if(Reverse){
    
    ModelList <- ModelList[rev(1:length(ModelList))]
    
    if(!is.null(ModelNames)){
      
      ModelNames %<>% rev
      
    }else{
      
      ModelNames <- length(ModelList):1
      
    }
    
  }
  
  if(is.null(ModelNames) & !is.null(names(ModelList))){
    
    ModelNames <- names(ModelList)
    
  }
  
  for(i in 1:length(ModelList)){
    
    model <- ModelList[[i]]
    
    if(any(class(model) %>% str_detect("inla"))){
      
      Graph <- as.data.frame(summary(model)$fixed)
      
      Graph %<>% rename(Estimate = mean,
                        Lower = `0.025quant`,
                        Upper = `0.975quant`)
      
      rownames(Graph) %<>% str_replace_all(":", "_")
      
    }
    
    if(any(class(model) %>% str_detect("MCMCglmm"))){
      Graph <- as.data.frame(summary(model)$solutions)
      colnames(Graph)[1:3] <- c("Estimate","Lower","Upper")
    }
    
    if(any(class(model) %>% str_detect("bam|gam"))){
      
      Graph  <-
        summary(model)[1:4] %>%
        map(~.x[1:length(summary(model)[[1]])]) %>%
        bind_cols() %>%
        rename(Estimate = p.coeff, P = p.pv) %>%
        # mutate(Lower = Estimate - se, Upper = Estimate + se) %>%
        as.data.frame
      
      Coefs <- coef(model)
      VC <- vcov(model)
      
      sim <- mvrnorm(1000, mu = Coefs, Sigma = VC)
      
      MCMCEffects <- sim %>% as.data.frame %>%
        map(~.x %>% as.mcmc %>% HPDinterval)
      
      rownames(Graph) <- summary(model)[[1]] %>% attr("names")
      
      Graph <- MCMCEffects[1:nrow(Graph)] %>%
        map(~tibble(Lower = .x[1],
                    Upper = .x[2])) %>%
        # bind_rows(.id = "Name") %>%
        bind_rows() %>%
        bind_cols(Graph, .)
      
    }
    
    if(any(class(model) == "ergm")){
      
      Summ = summary(model)
      CoefTable = as.data.frame(Summ$coef)
      CoefTable$Factor = rownames(CoefTable)
      
      Graph = CoefTable %>%
        dplyr::transmute(
          Estimate = Estimate,
          Lower = Estimate - 1.96 * `Std. Error`,
          Upper = Estimate + 1.96 * `Std. Error`,
          Factor = Factor
        )
    }
    
    Graph$Model <- i
    Graph$Factor <- rownames(Graph)
    
    Graphlist[[i]] <- Graph
    
  }
  
  Graph <- bind_rows(Graphlist)
  
  Graph %<>%
    mutate(Sig = ifelse(Lower*Upper>0, "*", "")) %>%
    mutate_at("Model", as.factor)
  
  if(!is.null(ModelNames)){
    
    levels(Graph$Model) <- ModelNames
    
  }
  
  position <- ifelse(length(unique(Graph$Model))  ==  1, "none", "right")
  
  if(is.null(VarOrder)) VarOrder <- rev(unique(Graph$Factor))
  if(is.null(VarNames)){
    
    VarNames <- c(VarOrder)
    names(VarNames) <- c(VarOrder)
    
  }
  
  Graph$Factor %<>% str_replace_all("(Intercept)", "Intercept")
  
  Graph$Factor <- factor(Graph$Factor, levels = VarOrder)
  
  levels(Graph$Factor) %<>% str_replace_all(VarNames)
  
  Graph %<>% as.data.frame %>% filter(!is.na(Factor))
  
  if(!Intercept){
    
    VarNames <- VarNames[!str_detect(VarNames, "ntercept")]
    
    Graph <- Graph %>% filter(Factor %in% VarNames)
    
  }
  
  Graph$starloc <- NA
  
  min <- min(Graph$Lower,na.rm = T)
  max <- max(Graph$Upper,na.rm = T)
  
  if(Sig == TRUE){
    
    Graph$starloc <- max + (max - min)/10
    
  }
  
  if(!is.null(StarLoc)){
    
    Graph$starloc <- StarLoc
    
  }
  
  Graph$Alpha <- with(Graph, ifelse(Lower*Upper>0, Alpha1, Alpha2))
  
  Graph %>%
    mutate(SigAlpha = factor(as.numeric(Lower*Upper > 0),
                             levels = c(1, 0))) ->
    
    Graph
  
  Graph %>% return
  
}
