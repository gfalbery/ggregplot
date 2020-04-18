
# INLA Mesh Check ####

GetMesh <- function(Mesh){

  Mesh$loc %>% as.data.frame() %>% rename(X = V1, Y = V2) %>% mutate(Vertex = 1:n()) -> Vertices

  Mesh$graph$vv %>% as.matrix %>% reshape2::melt() %>% rename(From = Var1, To = Var2) %>%
    filter(value == 1) %>% mutate(Group = 1:n()) %>% dplyr::select(-value) -> Edges

  Edges %>% gather("ToFrom", "Vertex", -Group) -> LongEdges

  LongEdges %>% left_join(Vertices, by = c("Vertex")) -> LongEdges

  return(

    list(

      Vertices = Vertices,

      Edges = LongEdges

    )

  )

}

ggMesh <- function(Mesh, Include = c("Edges")){

  GotMesh <- GetMesh(Mesh)

  MeshPlot <- GotMesh$Vertices %>%
    ggplot(aes(X, Y)) +
    coord_fixed()

  if("Edges" %in% Include){

    MeshPlot <- MeshPlot +
      geom_path(data = GotMesh$Edges, aes(group = Group))

  }

  if("Vertices" %in% Include){

    MeshPlot <- MeshPlot +
      geom_point()

  }

  return(MeshPlot)

}

ggMesh(Mesh)
