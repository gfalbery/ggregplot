
# GregTime ####

GregTime <- function(DecTime){

  HourTime <- as.numeric(DecTime)*24

  HourCharacter <- floor(HourTime)

  MinuteCharacter <- (HourTime - HourCharacter)*60

  HourCharacter <- HourCharacter %>% as.character %>%
    str_pad("0", side = "left", width = 2)

  H <- HourCharacter %>% str_pad("0", side = "left", width = 2)

  M <- MinuteCharacter %>% floor %>% str_pad("0", side = "left", width = 2)

  S <- MinuteCharacter %>% subtract(as.numeric(M)) %>%
    multiply_by(6000) %>% substr(1,2) %>% str_pad("0", side = "left", width = 2)

  paste(H, M, S, sep = ":")

}






