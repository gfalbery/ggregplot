
# Gretworks ####

Gretworks <- function(Data,
                      Columns = c("From", "To"),
                      Mode = "Uni",
                      Weighted = T,
                      Directed = F,
                      Scaled = F,
                      Nodes = "All",
                      Return = "Graph"){

  if(Mode == "Uni"){

    Data[,"From"] <- Data[,Columns[1]]
    Data[,"To"] <- Data[,Columns[2]]

    EdgeList <- Data[,c("From", "To")] %>% na.omit %>% as.matrix

    SocGraph <- graph_from_edgelist(EdgeList, directed = Directed)

  }

  if(Mode == "Bip"){

    Data[,"ID"] <- Data[,Columns[1]]
    Data[,"Group"] <- Data[,Columns[2]]

    M <- with(Data, table(ID, Group)) %>% as.matrix
    SocGraph <- graph.incidence(M, weighted = T)

  }

  if(Return == "Graph"){return(SocGraph)}else{

    if(Mode == "Uni"){

      AdjMatrix <- get.adjacency(SocGraph)

    }

    if(Mode == "Bip"){

      Proj <- bipartite.projection(SocGraph)$proj1

      AdjMatrix <- Proj %>% get.adjacency(attr = "weight") %>% as.matrix

      N <- nrow(AdjMatrix)
      A <- matrix(rep(rowSums(AdjMatrix), N), N)
      B <- matrix(rep(rowSums(AdjMatrix), each = N), N)
      AM <- AdjMatrix/(A + B - AdjMatrix)

    }


  }

  if(Return == "Proj"){

    return(Proj)

  }

  if(Return == "Adj"){

    if(Scaled){

      return(AM)

    }else{

      return(AdjMatrix)

    }
  }
}
