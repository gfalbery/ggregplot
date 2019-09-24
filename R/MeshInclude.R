#' MeshInclude
#'
#' @param xvar
#' @param yvar
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#'
#' @return
#'
#' @note This function does not seems to be done, therefore it is not exported.
#'
#' @examples
MeshInclude <- function(xvar, yvar, xmin, xmax, ymin, ymax){

  #TODO: check that the relation betwen xvar and xmin and xmax is correct..

  ifelse(xvar>xmin&
           xvar<xmax&
           yvar>ymin&
           yvar<ymax,
         1,
         0)

}
