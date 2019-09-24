#' Unwhich
#'
#' @param x result of [which()]
#' @param n size of the boolean mask to return
#'
#' @return A boolean mask representation of indices
#'
#' @note
#' - An idea could be to default `n` to be the maximum.
#' - Type-check that `x` is all integers
#' - This is currently unexplored
#'
#' @examples
#' x <- rnorm(10)
#' unwhich(x >= 0, 10)
#'
unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}
