#' Title
#'
#' @param df
#' @param unit
#'
#' @return
#' @export
#'
#' @rdname CumulativeAnimals
#'
#' @examples
CumulativeAnimals<-function(df,unit){

  df2<-data.frame(Unit=min(df[,unit],na.rm=T):max(df[,unit],na.rm=T),
                  Count=sapply(min(df[,unit],na.rm=T):max(df[,unit],na.rm=T),
         function(l) length(unique(df[df[,unit]<=l,"Animal"]))
  ))

  names(df2)[1]<-unit
  return(df2)
}
