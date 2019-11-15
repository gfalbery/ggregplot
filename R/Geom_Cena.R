
# Geom_cena() ####

library(tidyverse); library(ggplot2); library(ggimage); library(cowplot)

theme_set(theme_cowplot())

N <- 35

Data <- data.frame(X = rnorm(n = N, mean = 50, sd = 30),
                   #image = "~/ggregplot/Data/Picture1.png") %>%
                   image = "https://raw.githubusercontent.com/gfalbery/ggregplot/master/Data/Picture1.png") %>%
  mutate(Y = X + rnorm(n = N, mean = 0, sd = 25))

ggplot(Data, aes(X, Y)) +

  geom_point(colour = "white") +

  geom_image(aes(image=image), size = runif(N, 0, 0.15)) +
  lims(x = c(-10, 110), y = c(-10, 110))

geom_cena <- function(Sizes = 0.1){

  geom_image(aes(image = "~/ggregplot/Data/Picture1.png"),
             size = Sizes)

}

ggplot(Data, aes(X, Y)) + geom_cena(Sizes = runif(N, 0, 0.15)) + labs(y = "are you sure", x = "about that?")
