
# Establishing ggregplot functions

funcroot <- "~/R Projects/ggregplot/R"
source(paste0(funcroot, "/Model Effects Plot.R"))
source(paste0(funcroot, "/ggplot INLA Field.R"))
source(paste0(funcroot, "/INLA DIC Plot.R"))
source(paste0(funcroot, "/INLA Range Plot.R"))

library(devtools)


FunctionFiles <- as.list(paste0("D:/Scripy/Graveyard of Obscure Functions/",list.files("D:/Scripy/Graveyard of Obscure Functions")))

file.copy(FunctionFiles, funcroot)
