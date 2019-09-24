#' Linear Model for ggplot2
#'
#' Geom_smooth linear model by Hadley Wickham from official ggplot2 site
#' @rdname StatLm
StatLm <- ggplot2::ggproto("StatLm", Stat,
                  required_aes = c("x", "y"),

                  compute_group = function(data, scales) {
                    rng <- range(data$x, na.rm = TRUE)
                    grid <- data.frame(x = rng)

                    mod <- lm(y ~ x, data = data)
                    grid$y <- predict(mod, newdata = grid)

                    grid
                  }
)

#' Generete `StatLm`-layer
#'
#' @param mapping
#' @param data
#' @param geom
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#' @inheritParams ggplot2::layer
#'
#' @return
#' @export
#'
#' @rdname StatLM
#'
#' @examples
stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatLm, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
