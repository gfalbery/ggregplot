
#N1 <- NetworkCalc(Spouce, "Pittag.adj", "Column", "Row", 2^0.5, "Jandate", 1)

NetworkCalc <- function(Data, ID, Easting, Northing, Distance, Time, TimeLimit, lower.tri = F){

  AssMatrix <- table(Data[,ID], Data[,ID])

  Traps <- diag(AssMatrix)

  for(a in colnames(AssMatrix)){

    Locs <- Data[Data[,ID] == a, c(Easting, Northing, Time)]
    Locs$lTime <- Locs[,Time] - TimeLimit
    Locs$uTime <- Locs[,Time] + TimeLimit

    Assocs <- list()

    for(c in 1:nrow(Locs)){
      b <- Locs[c,]

      Lower <- as.numeric(b[4])
      Upper <- as.numeric(b[5])

      Timedf <- Data[Data[,Time]>Lower&Data[,Time]<Upper,]
      Timedf <- rbind(Timedf[which(apply(Timedf[,c(Easting, Northing, Time)], 1, function(a) all(a == b[1:3]))),],
                      Timedf[-which(apply(Timedf[,c(Easting, Northing, Time)], 1, function(a) all(a == b[1:3]))),])

      if(dim(Timedf)[1]>0){
        DistMat <- as.matrix(dist(Timedf[,c(Easting, Northing, Time)], diag = T))
        Codes <- Timedf[which(DistMat[,1] <= Distance),ID]
      }

      Assocs[[c]] <- Codes

    }

    AssMatrix[a,] <- table(unlist(Assocs))

    Data <- Data[!Data[,ID]==a,]

  }

  A <- matrix(rep(diag(AssMatrix), each = nrow(AssMatrix)), ncol = nrow(AssMatrix))
  Union <- A + t(A)
  AssMatrix <- AssMatrix /(Union - AssMatrix)

  if(lower.tri == TRUE){
    AssMatrix[lower.tri(AssMatrix)] <- AssMatrix[upper.tri(AssMatrix)]
  }

  return(AssMatrix)

}
