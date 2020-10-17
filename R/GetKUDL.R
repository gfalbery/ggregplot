
# Getting the KUDL data frame ####

GetKUDL <- function(KUDL){
  
  KUDL@coords %>% data.frame() %>% rename(X = Var2, Y = Var1) %>% mutate(Density = KUDL$ud)
  
}
