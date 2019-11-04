
# Converting to Camel Case ####

CamelConvert <- function(a){
  
  substr(a, 1, 1) <- substr(a, 1, 1) %>% toupper
  
  a
  
}