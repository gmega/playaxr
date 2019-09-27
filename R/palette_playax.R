#' A palette with a set of predefined colors
#'
#' palette_playax_XX expose a set of predefined colors which are either _assigned_
#' to existing streaming services (which can be checked with `palette_playax_services`.\
#' or are otherwise
#' _unassigned_ and can be reused for other purposes. These colors should be
#' used in time series, histograms, and any plots where their corresponding
#' streaming service arise, and completed with the unassigned colors if there
#' is a need to present extra information (e.g. data from other services).
#'
#' See \link{scale_color_playax} on how to access the Playax palette from
#' `ggplot2`.
#'
#' \describe{
#'    \item{`palette_playax()`}{Returns a named character vector where
#'    each name corresponds to a streaming service (e.g. `spotify`) and each value
#'    corresponds to an RGB color string.}
#'    \item{`palette_playax_unassigned()`}{Returns an unnamed character vector
#'    containing RGB color strings which are not assigned to any service.}
#'    \item{`palete_playax_all()`}{Returns an unnamed character vector with all
#'    of the colors in the palette, assigned or otherwise.}
#'    \item{`palette_playax_extended()`}{Returns a named character vector with
#'    all of the colors in `palette_playax_assigned()`, plus a set of unassigned
#'    colors picked from the palette and bound to the names passed in the
#'    `extension` parameter.}
#'    \item{`palette_playax_services()`}{Returns a character vector with the
#'    set of streaming services supported by the palette.}
#' }
#'
#' @param extension a character vector with extra service names.
#'
#' @rdname palette_playax
#' @export
palette_playax <- function() {
  PLAYAX_PALETTE[names(PLAYAX_PALETTE) != '']
}

#' @rdname palette_playax
#' @export
palette_playax_unassigned <- function() {
  unname(PLAYAX_PALETTE[names(PLAYAX_PALETTE) == ''])
}

#' @rdname palette_playax
#' @export
palette_playax_all <- function() {
  unname(PLAYAX_PALETTE)
}

#' @rdname palette_playax
#' @export
palette_playax_extended <- function(extension = c()) {
  assigned <- Filter(function(x) x != '', names(PLAYAX_PALETTE))
  unassigned <- length(PLAYAX_PALETTE) - length(assigned)
  if (length(extension) > unassigned) {
    stop(sprintf(
      'Palette has %d available slots but extension has %d elements.',
      unassigned, length(extension)))
  }
  # Copy before poking.
  palette <- PLAYAX_PALETTE
  extended_names <- c(assigned, extension)
  names(palette) <- extended_names
  palette[1:(length(extended_names))]
}

#' @rdname palette_playax
#' @export
palette_playax_services <- function() {
  names(palette_playax())
}

PLAYAX_PALETTE = c(
  facebook = '#4a90e2',
  youtube = '#d0021b',
  spotify = '#38a089',
  deezer = '#ff8484',
  instagram = '#f8e71c',
  '#f5a623',
  '#b8e986',
  '#dd3c6a'
)
