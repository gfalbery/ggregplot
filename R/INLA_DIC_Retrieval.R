#' Retrieving DIC from INLA models quickly ####
#'
#' @param model
#'
#' @return
#' @export
#'
#' @rdname INLA_misc
#'
#' @examples
MDIC <- function(model) {
  model$dic$dic
}
