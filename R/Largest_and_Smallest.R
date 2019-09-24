#' Largest and smallest
#'
#' @param vector
#' @param no
#' @param fun
#'
#' @export
#' @rdname math_misc
Largest <-function(vector, no = 5, fun = "raw"){

  ordvec <- tail(sort(vector), no)

  if(fun == "which")   which(vector%in%ordvec) else ordvec

  }

#' @param vector
#'
#' @param no
#'
#' @export
#' @rdname math_misc
Smallest <-function(vector, no = 5){
  which(vector%in%head(sort(vector), no))
}
