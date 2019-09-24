#' Non-problematic scale function
#'
#' Normalises a vector `x` such that mean is zero and variance is 1.
#'
#' Missing values are ignored by default.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @seealso For a version that works on `data.frame` see [scale()].
#'
#' @examples
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

