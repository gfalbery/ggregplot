
# GregTime ####

# when I run this code, specifying DecTime and Decs, it pops out the right answer
DecTime = 0.053
Decs = 6

DecTime <- 0.5

#gregtime <- function(DecTime, Decs=6) {

HourTime <- as.numeric(DecTime)*24
HourCharacter <- floor(HourTime)
MinuteCharacter <- (HourTime - HourCharacter)*60
HourCharacter <- HourCharacter %>% as.character %>%
  str_pad("0", side = "left", width = 2)
H <- HourCharacter
M <- MinuteCharacter  %>% floor %>% str_pad("0", side = "left", width = 2)
S <- MinuteCharacter  %>% subtract(as.numeric(M)) %>% multiply_by(6000)
if(Decs == 6){
  paste(H, M, S %>% substr(1,2),
        sep = ":")
}
#}








