
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

ggMesh(Mesh)



Vertices[,c("X", "Y")] %>% SpatialPointsDataFrame(data = ., coords = .) %>% st_as_sf()

Vertices[,c("X", "Y")] %>% chull -> Hull

Vertices[Hull,] -> Hull

Hull %>% dplyr::select(-c(X, Y, V3)) -> HullVertexNames

HullVertexNames %>% SpatialPointsDataFrame(data = ., coords = Hull[,c("X", "Y")]) %>% plot

Hull[,c("X", "Y")] %>% Polygon %>% list %>%
  Polygons(ID = '0') %>%
  list %>% SpatialPolygons -> Outline

plot(Outline)

Range <- 0.2

Outline %>% st_as_sf %>% st_buffer(Range) %>% plot

Hull[,c("X", "Y")] %>% slice(n():1) %>%
  Polygon(hole = T) %>% list %>%
  Polygons(ID = '0') %>%
  list %>% SpatialPolygons -> Outline

plot(Outline)

Range <- 0.2

Outline %>% st_as_sf %>% st_buffer(Range) %>% plot


CheckMeshBoundaries <- function(TestDf, Mesh, Range, HullColour = "black", ...){

  TestDF %>% chull -> Hull

  TestDF[Hull,] -> Hull

  Hull[,c("X", "Y")] %>%
    Polygon() %>% list %>%
    Polygons(ID = '0') %>%
    list %>% SpatialPolygons ->

    Outline

  Outline %>% st_as_sf %>% st_buffer(Range) -> Buffered
  Buffered$geometry[[1]][[1]] %>% as.data.frame() %>% rename(X = V1, Y = V2) -> BufferedDF

  MeshPlot <- ggMesh(Mesh, ...)

  MeshPlot +
    geom_path(data = BufferedDF, inherit.aes = F, aes(X, Y), colour = HullColour) +
    geom_point(data = TestDF, inherit.aes = F, aes(X, Y))

  # Mesh bit ####

  GotMesh <- GetMesh(Mesh, ...)

  GotMesh$Vertices[,c("X", "Y")]

    geom_polygon() +
    geom_path(data = BufferedDF, inherit.aes = F, aes(X, Y), colour = HullColour) +
    geom_point(data = TestDF, inherit.aes = F, aes(X, Y))

}

