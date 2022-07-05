# EfxPlotComp

Efxplot <- function(ModelList,
                    Sig = TRUE, StarLoc = NULL,
                    Alpha1 = 1, Alpha2 = 1,
                    PointOutline = T,
                    ModelNames = NULL,
                    VarNames = NULL, VarOrder = NULL,
                    Intercept = TRUE, PointSize = 1,
                    BarWidth = 0.1,
                    tips = 0.2){

  require(dplyr); require(ggplot2); require(INLA); require(MCMCglmm); require(MASS)

  Graphlist <- list()

  if(!class(ModelList) == "list"){
    ModelList <- list(ModelList)
  }

  if(is.null(ModelNames) & !is.null(names(ModelList))){

    ModelNames <- names(ModelList)

  }

  for(i in 1:length(ModelList)){
    model <- ModelList[[i]]

    if(class(model) == "inla"){

      Graph <- as.data.frame(summary(model)$fixed)
      colnames(Graph)[which(colnames(Graph)%in%c("0.025quant","0.975quant"))] <- c("Lower","Upper")
      colnames(Graph)[which(colnames(Graph)%in%c("0.05quant","0.95quant"))] <- c("Lower","Upper")
      colnames(Graph)[which(colnames(Graph)%in%c("mean"))] <- c("Estimate")

      rownames(Graph) %<>% str_replace_all(":", "_")

    }

    if(class(model) == "MCMCglmm"){
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

    Graph$Model <- i
    Graph$Factor <- rownames(Graph)

    Graphlist[[i]] <- Graph

  }

  Graph <- bind_rows(Graphlist)

  Graph$Sig <- with(Graph, ifelse(Lower*Upper>0, "*", ""))

  Graph$Model <- as.factor(Graph$Model)

  if(!is.null(ModelNames)){
    levels(Graph$Model) <- ModelNames
  }

  position <- ifelse(length(unique(Graph$Model))  ==  1, "none", "right")

  if(is.null(VarOrder)) VarOrder <- rev(unique(Graph$Factor))
  if(is.null(VarNames)){

    VarNames <- c(VarOrder)
    names(VarNames) <- c(VarOrder)

  }

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

  if(PointOutline){

    PointOutlineAlpha <- Alpha1

  }else{

    PointOutlineAlpha <- 0

  }

  ggplot(Graph,
         aes(x = as.factor(Factor),
             y = Estimate,
             group = Model,
             colour = Model,
             alpha = SigAlpha)) +
    geom_point(position = position_dodge(w = 0.5), size = PointSize) +
    geom_errorbar(position = position_dodge(w = 0.5),
                  aes(ymin = Lower, ymax = Upper), size = 0.3,
                  width = tips) +
    geom_hline(aes(yintercept = 0),lty = 2) + labs(x = NULL) + coord_flip() +
    theme(legend.position = position) +
    geom_text(aes(label = Sig, y = starloc),
              position = position_dodge(w = 0.5),
              show.legend = F) +
    scale_alpha_manual(values = c(Alpha1, Alpha2)) +
    guides(alpha = "none") +
    geom_point(colour = "black", aes(group = Model),
               position = position_dodge(w = 0.5), size = PointSize*(4/3),
               alpha = PointOutlineAlpha) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper, group = Model),
                  width = BarWidth,
                  position = position_dodge(w = 0.5),
                  colour = "black",
                  alpha = PointOutlineAlpha) +
    geom_point(position = position_dodge(w = 0.5), size = PointSize,
               alpha = PointOutlineAlpha)

}




