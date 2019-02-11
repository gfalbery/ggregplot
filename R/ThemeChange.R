# Theme Changing #####

ThemeChange <- function(){

  CurrentTheme <- theme_get()

  if(!identical(CurrentTheme, AlberTheme)){

    DefaultTheme <- theme_get()

    source("D:/Scripy/ggregplot/R/Establishing Themes and Palettes.R")

  }

  else print("Theme already set lol")

}

ThemeRevert <- function() theme_set(DefaultTheme)
