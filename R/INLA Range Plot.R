
# Deriving range of autocorrelation figures ####

INLARange <- function(ModelList, maxrange, Mesh, ModelNames = NULL){
  require(INLA)

  if(!class(ModelList)=="list"){
    ModelList <- list(ModelList)
  }

  SpFi.w = lapply(ModelList, function(j) inla.spde2.result(inla = j,
                                                           name = "w",
                                                           spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)),
                                                           do.transfer = TRUE))

  Kappa <- lapply(SpFi.w,  function(j)
    inla.emarginal(function(x) x,
                   j$marginals.kappa[[1]] ))

  d.vec <- seq(0, maxrange, length = 100)

  Cor<-lapply(Kappa,function(f){
    Cor.M <- as.numeric((f * d.vec) * besselK(f * d.vec, 1))
    Cor.M[1] <- 1
    return(data.frame(d.vec,Cor.M))
  })

  Cor <- dplyr::bind_rows(Cor)

  Cor$Model <- as.factor(rep(1:length(Kappa), each = 100))

  if(!is.null(ModelNames)){
    levels(Cor$Model) <- ModelNames
  }

  return(ggplot(Cor,
                aes(d.vec,Cor.M,colour = Model, lty = Model))+

           geom_line(size=1,alpha=0.8) + THEME + theme(strip.background = element_rect(fill="white"))+

           labs(colour="Model",x="Distance",y="Correlation") + coord_fixed(ratio = maxrange))

}

