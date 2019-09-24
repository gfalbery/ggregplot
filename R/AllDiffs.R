#' Deprecated
#'
#' Currently, returns `FALSE` all the time.
#'
#' @param Var1 A vector
#' @param Var2 Another vector
#' @param Lengths A boolean to return the lengths
#'
#' @return `Lengths`
#'
#' @note
#'
#' This function does not make sense, and used nowhere.
#'
AllDiffs <- function(Var1, Var2, Lengths = F){

  DiffList <- list()

  DiffList[[1]] <- setdiff(Var1, Var2)
  DiffList[[2]] <- intersect(Var1, Var2)
  DiffList[[3]] <- setdiff(Var2, Var1)

  if(Lengths) DiffList <- lapply(DiffList, length)

  return(Lengths)

}
