#' Quick distance
#'
#' @param Data
#' @param SubSample
#'
#' @return
#' @export
#'
#' @examples
Gristance <- function(Data, SubSample = F){

  if(is.numeric(SubSample)){

    if(SubSample>nrow(Data)) SubSample <- nrow(Data)

    Data %>% slice(sample(1:n(), SubSample) %>% sort) ->
      Data

  }

  if(nrow(Data)>2){

    Data %>% nrow %>% subtract(1) ->
      N

    sapply(1:(N), function(a){

      Data %>%
        slice(a:(a+1)) %>%
        dist()

    })

  }

  else{

    Data %>%
      dist()

  }

}
