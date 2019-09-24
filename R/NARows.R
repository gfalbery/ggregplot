#'  Removing NA Rows
#'
#' @param df
#' @param vars
#'
#' @return
#'
#' @note This function is effectively [stats::na.omit()], thus this is not
#' exported
#'
#' @examples
NARows <-function(df, vars){
  apply(as.data.frame(df[,vars]), 1, function(a){
    any(is.na(a)|a=="Inf"|a=="-Inf")
  })
}

