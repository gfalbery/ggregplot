
# AugmentEdges

# From here: https://github.com/thomasp85/tidygraph/issues/104

AugmentEdges <- function(Graph, Vars){
  
  for(Var in Vars){
    
    Graph %<>% 
      activate(edges) %>%
      mutate(NodeTypeFrom = .N()[[Var]][from],
             NodeTypeTo = .N()[[Var]][to]) %>% 
      rename_at(vars(contains("NodeType")), 
                ~str_replace(.x, "NodeType", Var))
    
  }
  
  Graph %>% return
  
}