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

  if(class(model) == "list"){

    model %>% map("dic") %>% map("dic")

  }else{

    model$dic$dic

  }
}
