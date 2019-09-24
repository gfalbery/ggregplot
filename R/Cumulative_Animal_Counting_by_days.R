#' Cumulative Animals
#'
#' @param df
#'
#' @return
#' @export
#'
#' @rdname CumulativeAnimals
#'
#' @note There are two version of this function, that does two things.
#'
#' @examples
CumulativeAnimals<-function(df){
  with(df,
  for(x in min(df$Day):(max(df$Day))){
    print(length(unique(df[!df$Animal%in%df[df$Day<x,"Animal"]&!df$Day>x,"Animal"]))+length(unique(df[df$Day<x,"Animal"])))
  }
  )
}
