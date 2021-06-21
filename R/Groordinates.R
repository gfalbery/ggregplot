
# Groordinates ####

library(tidyverse)

X <- runif(100)
Y <- runif(100)

Locations <- data.frame(X, Y)

Locations %>%
  ggplot(aes(X, Y)) +
  geom_point()

Dists <- dist(Locations) %>% as.matrix

Dists %>% dim

Dists %>% diag

i <- 1
j <- 2
k <- 3

IDs <- 1:nrow(Dists)

IDDF <- data.frame(ID = IDs, X = NA, Y = NA)

IDDF[i, c("X", "Y")] <- 0

IDDF[j, c("X")] <- Dists[i, j]
IDDF[j, c("Y")] <- 0

Xk <- (Dists[i, j]^2 + Dists[i, k]^2 - Dists[j, k]^2)/(2*Dists[i,j])
Yk <- abs(Dists[i, k]^2 - Xk^2)^0.5

IDDF[k, c("X")] <- Xk
IDDF[k, c("Y")] <- Yk

Groot <- function(x){

  if(x > 0){

    return(x^0.5)

  }else{

    return(-(abs(x)^0.5))

  }

}

a <- 4

for(a in 4:nrow(IDDF)){

  Xa <- (Dists[i, j]^2 + Dists[i, a]^2 - Dists[j, a]^2)/(2*Dists[i,j])
  Ya <- abs(Dists[i, a]^2 - Xa^2)^0.5

  Yak2 <- (Dists[a, k]^2 - (Xa - Xk)^2)

  Yak2>(Yk^2)

  if(Yak2^0.5 == (Ya + Yk)){

    Ya <- -Ya

  }

  # Xaj <- ()

  # if(Xaj == (Xa + Xk)){
  #
  #   Xa <- -Xa
  #
  # }

  IDDF[a, c("X")] <- Xa
  IDDF[a, c("Y")] <- Ya

}

IDDF %>%
  ggplot(aes(X, Y)) +
  geom_point() + coord_fixed()

Locations %>%
  ggplot(aes(X, Y)) +
  geom_point() + coord_fixed()

