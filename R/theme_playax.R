#' Playax theme for ggplot2 charts
#'
#' This theme requires Google's Open Sans fonts
#' \url{https://fonts.google.com/specimen/Open+Sans} to work properly.
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(tibble(x = 1:30, y = rnorm(30))) +
#'   geom_line(aes(x = x, y = y)) +
#'   theme_playax()
#'
#' @export
theme_playax <- function(font_size = 12) {
  theme_gray() + theme(
    text = element_text(family = 'Open Sans', size = font_size),
    axis.text = element_text(size = font_size),
    axis.title = element_text(size = font_size),
    legend.text = element_text(size = font_size),
    panel.background = element_rect(fill = NA, colour = NA),
    panel.grid.major.y = element_line(size = rel(1), colour = '#e6e6e6'),
    panel.grid.minor.y = element_blank()
  )
}

#' @export
big_fonts <- function(n = 12) {
  list(
    theme(
      axis.text = element_text(size = n),
      axis.title = element_text(size = n),
      text = element_text(size = n),
      legend.text = element_text(size = n)
    )
  )
}
