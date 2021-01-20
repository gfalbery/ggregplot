
# Grimport ####

Grimport <- function(Dir, Str = "", Function = NULL){

  Files <- Dir %>% list.files(full.names = T, pattern = Str)

  if(is.null(Function)){

    if(str_detect(Str, ".rds")){

      Files %>% map(readRDS) -> FileList

    }

    if(str_detect(Str, ".csv")){

      Files %>% map(read.csv) -> FileList

    }

  }else{

    Files %>% map(Function) -> FileList

  }

  names(FileList) <- Files %>% str_split("/") %>% map_chr(last)

  return(FileList)

}
