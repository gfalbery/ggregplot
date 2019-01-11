# Quick workspace save ####

SafeSave <- function(){
  require(dplyr)

  WD <- getwd()
  SafeFolder <- paste0("~/R Workspaces/",last(strsplit(getwd(), "/")[[1]]))

  if(!dir.exists(SafeFolder)) dir.create(SafeFolder)

  save.image(paste0(SafeFolder,"/.Rdata")) # Saves workspace on C:/
  file.copy(from = paste0(SafeFolder,"/.Rdata"),
            to = paste0(WD,"/.Rdata"),
            overwrite = TRUE) # Moves workspace to D:/

}
