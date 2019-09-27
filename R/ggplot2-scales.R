#' @export
scale_fill_playax <- function(..., aesthetics = 'fill',
                              palette = palette_playax()) {
  scale_fill_manual(values = palette, aesthetics = aesthetics, ...)
}

#' @export
scale_color_playax <- function(..., aesthetics = 'color',
                               palette = palette_playax()) {
  scale_fill_manual(values = palette, aesthetics = aesthetics, ...)
}

#' @export
scale_y_abbrv <- function(...) {
  scale_y_continuous(labels = abbrv_large)
}

#' @export
scale_x_abbrv <- function(...) {
  scale_x_continuous(labels = abbrv_large)
}
