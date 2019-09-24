#' Quick workspace save
#'
#' This saves to hard-drive. Use with care.
#'
#' @return Nothing
#'
#' @note This function contains personal paths. Therefore it is not exported.
#' Use with care.
#'
#' @examples
SafeSave <- function(){
  # require(dplyr)
  requireNamespace("dplyr", quietly = TRUE)

  WD <- getwd()
  SafeFolder <- paste0("~/R Workspaces/", dplyr::last(strsplit(getwd(), "/")[[1]]))

  if(!dir.exists(SafeFolder)) dir.create(SafeFolder)

  save.image(paste0(SafeFolder,"/.Rdata")) # Saves workspace on C:/
  file.copy(from = paste0(SafeFolder,"/.Rdata"),
            to = paste0(WD,"/.Rdata"),
            overwrite = TRUE) # Moves workspace to D:/

}
