
# Establishing ggregplot functions

funcroot <- "~/R Projects/ggregplot/R"

FunctionFiles <- as.list(paste0("D:/Scripy/Graveyard of Obscure Functions/",
                                list.files("D:/Scripy/Graveyard of Obscure Functions")))

file.copy(FunctionFiles, funcroot)

FuncRemove <-paste0(funcroot,"/","Contact Network Gen",".R")

file.remove(FuncRemove)
