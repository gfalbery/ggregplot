#' nunique
#'
#' Length of unique entries
#'
#' @param x a numeric vector
#'
#' @return no. of unique entries in `x`
#' @export
#'
nunique <- function(x) {
  length(unique(x))
}
