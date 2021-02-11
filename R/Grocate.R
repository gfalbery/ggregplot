
# Grocate ####
# Reconstruct bivariate locations from a distance matrix #####

library(tidyverse); library(ggregplot); library(magrittr)

DM <- read.csv("data/geographic_distance_df.csv")

NRow <- nrow(DM)

DM %<>% rename(From = 1, To = 2)

IDs <- DM %>% dplyr::select(1, 2) %>% unlist %>% unique

ID1 <- IDs[1]
ID2 <- IDs[2]

Locations <- data.frame(
  
  ID = ID1, X = 0, Y = 0
  
) %>% 
  bind_rows(
    
    data.frame(
      
      ID = ID2, Y = 0,
      X = DF %>% filter()
      
    )
    
  )


