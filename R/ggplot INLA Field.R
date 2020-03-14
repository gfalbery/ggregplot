# ggplot for INLA fields ####

ggField <- function(Model, Mesh, Groups = 1, Fill = "Discrete",
                    Boundary = NULL, BoundaryWidth = 1,
                    Res = 300,
                    Points = NULL, PointAlpha = 1,
                    PointColour = "black"){#, xlim, ylim){

  require(ggplot2); require(INLA); require(tidyverse)

  Projection <- inla.mesh.projector(Mesh,
                                    dims = c(Res, Res))

  Full.Projection <- expand.grid(x = Projection$x, y = Projection$y)

  Dim1 <- dim(Full.Projection)[1]

  if(Groups == 1){

    Full.Projection$value <- c(inla.mesh.project(Projection, Model$summary.random$w$mean))

  }else{

    Full.Projection[,paste0("Group",1:Groups)]<-apply(
      matrix(Model$summary.random$w$mean,ncol=Groups), 2,
      function(x) c(inla.mesh.project(Projection,x)))

    Full.Projection <-
      reshape2::melt(Full.Projection, id.vars = c(names(Full.Projection)[-which(names(Full.Projection)%in%paste0("Group",1:Groups))]))

  }

  if(Fill == "Discrete"){

    Full.Projection$Fill <- cut(Full.Projection$value,
                                breaks = quantile(Full.Projection$value, 0:9/9, na.rm = T),
                                labels = c(round(quantile(Full.Projection$value, 0:9/9, na.rm = T), 2)[1:9]),
                                include.lowest = T)
  } else{

    Full.Projection$Fill <- Full.Projection$value

  }

  Full.Projection$Group <- rep(1:Groups, each = Dim1)

  Full.Projection <- na.omit(Full.Projection)

  FieldPlot <- ggplot(Full.Projection,aes(x, y))

  if(!is.null(Boundary)){
    names(Boundary)[1] <- "x"
    names(Boundary)[2] <- "y"
    FieldPlot <- FieldPlot + geom_polygon(data = Boundary, fill = "white")
  }

  FieldPlot <- FieldPlot +
    geom_tile(colour = "black") +
    geom_tile(colour = NA, aes(fill = Fill)) +
    coord_fixed() + labs(fill = "Mean") +
    labs(x = "Easting", y = "Northing")

  if(Fill == "Discrete"){

    FieldPlot <- FieldPlot + guides(fill = guide_legend(reverse = T))

  }

  if(!is.null(Boundary)){
    FieldPlot <- FieldPlot +
      geom_polygon(data = Boundary,
                   fill = NA,
                   colour = "black", size = BoundaryWidth,
                   inherit.aes = F,
                   aes(x, y))
  }

  if(Groups>1) FieldPlot <- FieldPlot + facet_wrap( ~ Group) + theme(strip.background = element_rect(fill = "white"))

  if(!is.null(Points)){

    Points$X <- Points[,1]
    Points$Y <- Points[,2]

    FieldPlot <- FieldPlot +
      geom_point(data = Points, inherit.aes = F, aes(X, Y), alpha = PointAlpha, colour = PointColour)

  }

  return(FieldPlot)

}
