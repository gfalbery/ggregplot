
# Deriving range of autocorrelation figures ####

INLARange <- function(ModelList, MaxRange, MeshList,
                      ModelNames = NULL,
                      Priors = NULL, PriorProbabilities = NULL,
                      WNames = "w",
                      Return = "Figure",
                      Resolution = 100){

  require(INLA)

  if(is.null(Priors)){

    Priors <- MaxRange/2

    PriorProbabilities <- 0.5

  }

  if(!class(ModelList)=="list"){

    ModelList <- list(ModelList)

  }

  if(!class(MeshList)=="list"){

    MeshList <- list(MeshList)

  }

  if(length(Priors) == 1) {

    Priors <- rep(Priors, length(ModelList))

    PriorProbabilities <- rep(PriorProbabilities, length(ModelList))

  }

  if(length(WNames) == 1) {

    WNames <- rep(WNames, length(ModelList))

  }

  SpFi.w = 1:length(ModelList) %>% lapply(function(j){
    inla.spde2.result(inla = ModelList[[j]],
                      name = WNames[[j]],
                      spde = inla.spde2.pcmatern(mesh = MeshList[[j]],
                                                 prior.range = c(Priors[[j]], PriorProbabilities[[j]]),
                                                 prior.sigma = c(.5, .5)),
                      do.transfer = TRUE)
  })

  Kappa <- lapply(SpFi.w,  function(j)
    inla.emarginal(function(x) x,
                   j$marginals.kappa[[1]] ))

  d.vec <- seq(0, MaxRange, length = Resolution)

  Cor<-lapply(Kappa,function(f){
    Cor.M <- as.numeric((f * d.vec) * besselK(f * d.vec, 1))
    Cor.M[1] <- 1
    return(data.frame(d.vec,Cor.M))
  })

  Cor <- dplyr::bind_rows(Cor)

  Cor$Model <- as.factor(rep(1:length(Kappa), each = Resolution))

  if(!is.null(ModelNames)){
    levels(Cor$Model) <- ModelNames
  }

  ReturnList <- list()

  if("Figure" %in% Return){

    ReturnList$Figure <- ggplot(Cor, aes(d.vec, Cor.M, colour = Model, lty = Model)) +

      geom_line(size = 1, alpha = 0.8) + coord_fixed(ratio = MaxRange) +

      labs(colour ="Model",x = "Distance", y = "Correlation")
  }

  if("Data" %in% Return){

    ReturnList$Data <- Cor

  }

  if("Kappa" %in% Return){

    ReturnList$Kappa <- unlist(Kappa)

  }

  if(length(Return)>1){

    return(ReturnList)

  }else{

    return(ReturnList[[1]])

  }

}

