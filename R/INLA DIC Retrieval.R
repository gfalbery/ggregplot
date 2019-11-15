# Retrieving DIC from INLA models quickly ####

MDIC <- function(model) {

  if(class(model) == "list"){

    model %>% map("dic") %>% map("dic")

  }else{

    model$dic$dic

  }
}
