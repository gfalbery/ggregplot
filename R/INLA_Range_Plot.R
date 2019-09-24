#' Deriving range of autocorrelation figures
#'
#' @param ModelList
#' @param MaxRange
#' @param MeshList
#' @param ModelNames
#'
#' @return
#' @export
#'
#' @rdname INLA_misc
#'
#' @examples
INLARange <- function(ModelList, MaxRange, MeshList, ModelNames = NULL){
  require(INLA)

  if(!class(ModelList)=="list"){
    ModelList <- list(ModelList)
  }

  if(!class(MeshList)=="list"){
    MeshList <- list(MeshList)
  }

  SpFi.w = 1:length(ModelList) %>% lapply(function(j){
    inla.spde2.result(inla = ModelList[[j]],
                      name = "w",
                      spde = inla.spde2.pcmatern(mesh = MeshList[[j]], prior.range = c(10, 0.5), prior.sigma = c(.5, .5)),
                      do.transfer = TRUE)
  })

  Kappa <- lapply(SpFi.w,  function(j)
    inla.emarginal(function(x) x,
                   j$marginals.kappa[[1]] ))

  d.vec <- seq(0, MaxRange, length = 100)

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
                aes(d.vec,Cor.M,colour = Model, lty = Model)) +

           geom_line(size=1,alpha=0.8) + coord_fixed(ratio = MaxRange) +

           labs(colour ="Model",x = "Distance", y = "Correlation"))

}

