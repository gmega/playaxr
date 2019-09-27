#' @export
scale_fill_playax <- function(..., aesthetics = 'fill',
                              mode = 'unnamed', extension = c()) {
  scale_fill_manual(values = palette_playax(mode, extension),
                    aesthetics = aesthetics, ...)
}

#' @export
scale_color_playax <- function(..., aesthetics = 'color',
                               mode = 'unnamed', extension = c()) {
  scale_fill_manual(values = palette_playax(mode, extension),
                    aesthetics = aesthetics, ...)
}

#' @export
scale_y_abbrv <- function(...) {
  scale_y_continuous(labels = abbrv_large)
}

#' @export
scale_x_abbrv <- function(...) {
  scale_x_continuous(labels = abbrv_large)
}
