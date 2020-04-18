
# INLA Mesh Check ####

GetMesh <- function(Mesh){

  Mesh$loc %>% as.data.frame() %>% rename(X = V1, Y = V2) %>% mutate(Vertex = 1:n()) -> Vertices

  Mesh$graph$vv %>% as.matrix %>%
    reshape2::melt() %>%
    rename(From = Var1, To = Var2) %>%
    filter(value == 1) %>%
    mutate(Group = 1:n()) %>%
    dplyr::select(-value) ->

    Edges

  Edges %>% gather("ToFrom", "Vertex", -Group) ->

    LongEdges

  LongEdges %>% left_join(Vertices, by = c("Vertex")) ->

    LongEdges

  return(

    list(

      Vertices = Vertices,

      Edges = LongEdges

    )

  )

}

ggMesh <- function(Mesh, Include = c("Edges"), Alpha = 1){

  GotMesh <- GetMesh(Mesh)

  MeshPlot <- GotMesh$Vertices %>%
    ggplot(aes(X, Y)) +
    coord_fixed()

  if("Edges" %in% Include){

    MeshPlot <- MeshPlot +
      geom_path(data = GotMesh$Edges, aes(group = Group),
                alpha = Alpha)

  }

  if("Vertices" %in% Include){

    MeshPlot <- MeshPlot +
      geom_point(alpha = Alpha)

  }

  return(MeshPlot)

}

CheckMeshBoundaries <- function(Locations,
                                Mesh, Range,
                                HullColour = "black",
                                PointColour = "black",
                                ...){

  Locations %>% chull -> Hull

  Locations[Hull,] -> Hull

  Hull[,c("X", "Y")] %>%
    Polygon() %>% list %>%
    Polygons(ID = '0') %>%
    list %>% SpatialPolygons %>%
    st_as_sf %>% st_buffer(Range) -> Buffered

  # How is this the easiest way to do this ####

  Buffered$geometry[[1]][[1]] %>% as.data.frame() %>% rename(X = V1, Y = V2) -> BufferedDF

  MeshPlot <- ggMesh(Mesh, ...)

  MeshPlot +
    geom_path(data = BufferedDF, inherit.aes = F, aes(X, Y), colour = HullColour) +
    geom_point(data = Locations, inherit.aes = F, aes(X, Y))

}

