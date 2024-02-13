# ggplot for INLA fields ####

ggField <- function(Model, Mesh,
                    Groups = 1, GroupVar = NULL, GroupLabels = NULL,
                    Fill = "Discrete", FillAlpha = F,
                    FitScale = "Link", GrandMean = NULL, GrandSD = NULL, Round = "Before",
                    Boundary = NULL, BoundaryWidth = 1,
                    Res = 300,
                    Points = NULL, PointAlpha = 1, PointSize = NULL,
                    PointColour = "black"){#, xlim, ylim){

  require(ggplot2); require(INLA); require(tidyverse)

  Projection <- inla.mesh.projector(Mesh,
                                    dims = c(Res, Res))

  Full.Projection <- expand.grid(x = Projection$x, y = Projection$y)

  Dim1 <- nrow(Full.Projection)

  WName <- Model$summary.hyperpar %>% rownames

  WName[str_detect(WName, "Range for")] %>%
    str_split(" ") %>% map_chr(last) ->
    WName

  if(Groups == 1){

    Full.Projection$value <- c(inla.mesh.project(Projection, Model$summary.random$w$mean))

  }else{

    Full.Projection[,paste0("Group",1:Groups)] <-

      apply(matrix(Model$summary.random[[WName]]$mean, ncol = Groups), 2,

            function(x) c(inla.mesh.project(Projection, x)))

    Full.Projection <-
      reshape2::melt(Full.Projection,
                     id.vars = c(names(Full.Projection)[-which(names(Full.Projection)%in%paste0("Group",
                                                                                                1:Groups))]))

  }

  Full.Projection$Group <- rep(1:Groups, each = Dim1)

  if(length(GrandMean) == Groups){

    GrandMean %<>% rep(each = Dim1)

  }

  if(FitScale == "Binomial" & Round == "Before"){

    Full.Projection %<>%
      mutate_at("value", ~logistic(as.numeric(as.character(.x)) + GrandMean))

  }

  if(FitScale == "Gaussian" & Round == "Before"){

    Full.Projection %<>%
      mutate_at("value", ~(as.numeric(as.character(.x)))*GrandSD + GrandMean)

  }

  if(Fill == "Discrete"){

    Full.Projection$Fill <- cut(Full.Projection$value,
                                breaks = quantile(Full.Projection$value, 0:9/9, na.rm = T),
                                labels = c(round(quantile(Full.Projection$value, 0:9/9, na.rm = T), 2)[1:9]),
                                include.lowest = T)
  } else{

    Full.Projection$Fill <- Full.Projection$value

  }

  if(FitScale == "Gaussian" & Round == "After"){

    Full.Projection %<>%
      mutate_at("Fill", ~(as.numeric(as.character(.x)))*GrandSD + GrandMean)

  }

  if(FitScale == "Binomial" & Round == "After"){

    Full.Projection %<>%
      mutate_at("value", ~logistic(as.numeric(as.character(.x)) + GrandMean))

    if(Fill == "Discrete"){

      Full.Projection %<>%
        mutate_at("Fill", ~factor(.x, levels = gtools::mixedsort(unique(.x))))

    }

  }

  Full.Projection <- na.omit(Full.Projection)

  FieldPlot <- ggplot(Full.Projection,aes(x, y))

  if(!is.null(Boundary)){

    names(Boundary)[1] <- "x"
    names(Boundary)[2] <- "y"
    FieldPlot <- FieldPlot + geom_polygon(data = Boundary, fill = "white")

  }

  if(!FillAlpha){

    FieldPlot <- FieldPlot +
      # geom_tile(colour = "black") +
      geom_tile(colour = NA, aes(fill = Fill)) +
      coord_fixed() + labs(fill = "Mean") +
      labs(x = "Easting", y = "Northing")

  }else{

    FieldPlot <- FieldPlot +
      #geom_tile(colour = "black") +
      geom_tile(colour = NA, aes(fill = Fill, alpha = Fill)) +
      coord_fixed() + labs(fill = "Mean") +
      labs(x = "Easting", y = "Northing")

  }

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

  if(Groups>1){

    if(!is.null(GroupLabels)){

      FacetLabels <- GroupLabels

    }else FacetLabels <- as.character(1:Groups)

    names(FacetLabels) <- as.character(1:Groups)

    FieldPlot <- FieldPlot + facet_wrap( ~ Group, labeller = as_labeller(FacetLabels)) +
      theme(strip.background = element_rect(fill = "white"))

  }

  if(!is.null(Points)){

    Points$X <- Points[,1]
    Points$Y <- Points[,2]

    if(Groups == 1){

      FieldPlot <- FieldPlot +
        geom_point(data = Points, inherit.aes = F, aes(X, Y), alpha = PointAlpha, colour = PointColour)

    }else{

      Points$Group <- Points[,GroupVar] %>% as.factor %>% as.numeric

      FieldPlot <- FieldPlot +
        geom_point(data = Points, inherit.aes = F,
                   aes(X, Y, group = Group),
                   alpha = PointAlpha, colour = PointColour, size = PointSize)

    }
  }

  return(FieldPlot)

}
