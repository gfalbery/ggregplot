
# INLA Mesh Check ####

Mesh <- MeshList[[1]]

Mesh %>% plot

"meta"     "manifold" "n"        "loc"      "graph"    "segm"     "idx"      "crs"

Mesh[[1]]

Mesh[[5]][[3]]

Mesh$loc %>% as.data.frame() %>% rename(X = V1, Y = V2) %>% mutate(Vertex = 1:n()) -> Vertices

Vertices %>%
  ggplot(aes(X, Y)) + geom_point() + geom_text(aes(label = Vertex))

Mesh$n

max(Vertices$Vertex)

Mesh$graph %>% names

Mesh$graph$vv %>% as.matrix %>% reshape2::melt() %>%
  filter(value == 1) %>% mutate(Group = 1:n()) %>% dplyr::select(-value) -> Edges

Edges %>% gather("RowCol", "Vertex", -Group) -> LongEdges

LongEdges %>% left_join(Vertices, by = c("Vertex")) -> LongEdges

LongEdges$Group %>% table() %>% table()

Vertices %>%
  ggplot(aes(X, Y)) +
  #geom_point() +
  #geom_polygon(data = LongEdges, aes(group = Group), fill = NA) +
  geom_path(data = LongEdges, aes(group = Group)) +
  coord_fixed()




