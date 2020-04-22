
# Extracting model DIC tables from model addition functions ####

DICTableGet <- function(dDICList){

  dDICList %>%

  map_dfr(~data.frame(Variable = as.character(names(.x)), Delta = .x) %>%
            mutate(Kept = as.numeric((Delta == min(Delta))&Delta < -2)) %>%
            mutate_at("Variable", ~ ifelse(Kept, paste0("**", as.character(.x), "**"), as.character(.x))),
          .id = "Round")

}
