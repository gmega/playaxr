#' scales for Playax plots in \link{ggplot2}
#'
#' This package provides:
#' \itemize{
#'   \item `color` and `fill`` scales based on \link{palette_playax};
#'   \item `x` and `y` scales which abbreviate large numbers with \link{abbr_large}.
#' }
#'
#' @examples
#'
#' data <- data.frame(
#'   day = rep(1:20, 3),
#'   metric = do.call(c, lapply(1:3, function(x) cumsum(rnorm(20, sd = 1e7)))),
#'   service = rep(c('facebook', 'instagram', 'spotify'), each = 20)
#' )
#'
#' ggplot(data) +
#'   geom_line(aes(x = day, y = metric, col = service)) +
#'   theme_playax() +
#'   scale_color_playax() +
#'   scale_y_abbrv()
#'
#' data <- rbind(
#'   data,
#'   data.frame(
#'     day = 1:20,
#'     metric = cumsum(rnorm(20, sd = 1e7)),
#'     service = rep('other', 20)
#'   )
#' )
#'
#' ggplot(data) +
#'   geom_line(aes(x = day, y = metric, col = service)) +
#'   theme_playax() +
#'   scale_color_playax(palette_playax_extended(extension = c('other'))) +
#'   scale_y_abbrv()
#'
#' @export
scale_fill_playax <- function(..., aesthetics = 'fill',
                              palette = palette_playax()) {
  scale_fill_manual(values = palette, aesthetics = aesthetics, ...)
}

#' @rdname scale_fill_playax
#' @export
scale_color_playax <- function(..., aesthetics = 'color',
                               palette = palette_playax()) {
  scale_fill_manual(values = palette, aesthetics = aesthetics, ...)
}

#' @rdname scale_fill_playax
#' @export
scale_y_abbrv <- function(...) {
  scale_y_continuous(labels = abbrv_large)
}

#' @rdname scale_fill_playax
#' @export
scale_x_abbrv <- function(...) {
  scale_x_continuous(labels = abbrv_large)
}
