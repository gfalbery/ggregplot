#' Mathematical convenience functions
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' @rdname math_misc
Prev <- function(x){

  length(x[x>0])/length(x)

}

#' @export
#' @rdname math_misc
#' @note These function are represented as `*logis` functions.
logistic <- function(a) exp(a)/(1 + exp(a))
#' @export
#' @rdname math_misc
logit <- function(a) log(a/(1-a))
