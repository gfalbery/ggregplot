# Largest and smallest ####

Largest <-function(vector, no = 5, fun = "raw"){

  ordvec <- tail(sort(vector), no)

  if(fun == "which")   which(vector%in%ordvec) else ordvec

  }

Smallest <-function(vector, no = 5){
  which(vector%in%head(sort(vector), no))
}
