DEFAULT_FONTSIZE <- 12
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
theme_playax <- function() {
  theme_gray() +
    theme(
      text = element_text(family = 'Open Sans'),
      legend.key = element_rect(colour = NA, fill = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      panel.grid.major.y = element_line(size = rel(1), colour = '#e6e6e6'),
      panel.grid.minor.y = element_blank()
    ) +
    big_fonts(DEFAULT_FONTSIZE)
}

#' Playax "black" theme for ggplot2 charts
#'
#' This theme requires Google's Open Sans fonts
#' \url{https://fonts.google.com/specimen/Open+Sans} to work properly.
#'
#' @export
theme_playax_black <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = 'Open Sans', color = 'white', face = 'bold'),
      legend.key = element_rect(colour = NA, fill = NA),
      plot.background = element_rect(fill = '#262626'),
      panel.background = element_rect(fill = '#262626', colour = NA),
      axis.ticks = element_line(colour = 'white'),
      axis.text = element_text(colour = 'white'),
      strip.text = element_text(colour = 'white'),
      # Playax black is completely gridless.
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    big_fonts(DEFAULT_FONTSIZE)
}

#' @export
big_fonts <- function(n = DEFAULT_FONTSIZE) {
  list(
    theme(
      axis.text = element_text(size = n),
      axis.title = element_text(size = n),
      text = element_text(size = n),
      legend.text = element_text(size = n),
      strip.text = element_text(size = n)
    )
  )
}
