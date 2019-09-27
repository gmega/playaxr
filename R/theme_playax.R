#' Playax theme for ggplot2 charts
#'
#' This theme requires Google's Open Sans fonts
#' \url{https://fonts.google.com/specimen/Open+Sans} to work properly.
#'
#' @examples
#'
#' ggplot(tibble(x = 1:30, y = rnorm(30))) +
#'   geom_line(aes(x = x, y = y)) +
#'   theme_playax()
#'
#' @export
theme_playax <- function() {
  theme_gray() + theme(
    text = element_text(family = 'Open Sans'),
    panel.background = element_rect(fill = NA, colour = NA),
    panel.grid.major.y = element_line(size = rel(1), colour = '#e6e6e6'),
    panel.grid.minor.y = element_blank()
  )
}