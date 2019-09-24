#' Plot to compare matrix values
#'
#' @param Matrix1
#' @param Matrix2
#' @param Names
#' @param Axes
#'
#' @return
#' @export
#'
#' @examples
MatrixPoints <- function(Matrix1, Matrix2, Names = NULL, Axes = NULL){

  require(Matrix)

  if(is.null(Names)) Names <- intersect(rownames(Matrix1), rownames(Matrix2))

  Witch <- which(lower.tri(Matrix1[Names, Names]))

  v1 <- Matrix1[Names, Names] %>% as.matrix() %>% c
  v2 <- Matrix2[Names, Names] %>% as.matrix() %>% c

  v1 <- v1[Witch]
  v2 <- v2[Witch]

  CorPlot <- qplot(v1, v2)

  if(!is.null(Axes)) CorPlot <- CorPlot + labs(x = Axes[1], y = Axes[2])

  CorPlot

}
