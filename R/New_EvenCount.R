#' Evencount
#'
#' @param df A data.frame that contains `Samples` and `Day`
#'
#' @return A [ggplot2::ggplot()] object
#' @export
#'
#' @examples
Evencount <- function(df) {
  df$Count <- 0
  j <- 1
  df$Gradient <- df$Samples / df$Day

  for (i in 2:length(df$Day)) {
    if (df[i, "Gradient"] < df[(i - 1), "Gradient"]) {
      df[j:(i - 1), "Count"] <- max(df[j:i, "Gradient"]) * j:(i - 1)
      j <- i
      df[j:length(df$Day), "Gradient"] <-
        (df[j:length(df$Day), "Samples"] - df[(i - 1), "Samples"]) / df[j:length(df$Day), "Day"]
    } else{
      df[j:i, "Count"] <- max(df[j:i, "Gradient"]) * j:i
    }
  }
  df[length(df$Day), "Count"] <- df[length(df$Day), "Samples"]

  # require(ggplot2)
  requireNamespace("ggplot2", quietly = TRUE)
  ggplot2::ggplot(df, ggplot2::aes(Day, Count)) + ggplot2::geom_line()

}
