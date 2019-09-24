#' Theme Changing
#'
#'
#' @return Nothing
#' @export
#'
#' @note
#'
#'
#' This function should have an include tag for "Establishing Themes and Palettes.R"
#'
ThemeChange <- function(){

  CurrentTheme <- theme_get()

  if(!identical(CurrentTheme, AlberTheme)){

    DefaultTheme <- theme_get()

    source("D:/Scripy/ggregplot/R/Establishing Themes and Palettes.R")

  }

  else print("Theme already set lol")

}

#' ThemeRevert
#'
#' Needs fixing
#'
#' @export
#'
#' @note
#'
#' This function should return to the theme that was present before this function was
#' invoked, i.e. using `CurrentTheme`.
#'
#' @rdname  ThemeChange
ThemeRevert <- function() theme_set(DefaultTheme)
