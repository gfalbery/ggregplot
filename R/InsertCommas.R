
# Insert Commas ####

InsertCommas <- function(String){

  String %>% sapply(function(a){

    Chars <- nchar(a)

    if(Chars > 3){

      Ceiling <- ceiling(Chars/3)

      Letters <- a %>% str_split("") %>% unlist %>% rev

      1:Ceiling %>% lapply(function(b){

        Letters[3:1 + ((b-1)*3)] %>% na.omit %>% paste0(collapse = "")

      }) %>% rev() %>% unlist %>% paste(collapse = ",") %>%
        return

    }else{

      a %>% return

    }

  })

}
