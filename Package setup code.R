
# Establishing ggregplot functions ####

# Moving functions from D drive to package folder ####

funcroot <- "~/R Projects/ggregplot/R"

FunctionFiles <- as.list(paste0("D:/Scripy/Graveyard of Obscure Functions/",
                                list.files("D:/Scripy/Graveyard of Obscure Functions")))

file.copy(FunctionFiles, funcroot)

# Removing files that have changed ####
FuncRemove <-paste0(funcroot,"/","Contact Network Gen",".R")

file.remove(FuncRemove)

# Checking that they all work ####

FileEnds <- list.files("D:/Scripy/Graveyard of Obscure Functions")
names(FileEnds) <- list.files("D:/Scripy/Graveyard of Obscure Functions")
FileEnds <- FileEnds[-which(FileEnds=="Old Code")]

for(x in paste0(funcroot,"/",
                FileEnds)){
  print(x)
  source(x)
}


