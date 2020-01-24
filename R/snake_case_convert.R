
# SnakeConvert ####

# from this tweet https://twitter.com/coolbutuseless/status/1173587086307287046 ###

snake_case_convert <- function(Word){
  
  tolower(gsub('(?<!^)([A-Z]', '_\\1', Word, perl = TRUE))
  
}
  
