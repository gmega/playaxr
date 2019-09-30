#' Abbreviated labels for \href{https://en.wikipedia.org/wiki/Names_of_large_numbers}{large numbers}.
#'
#' `abbrv_large` takes a numeric vector and returns a character vector containing
#'  abbreviated names of the form `DDDC` where `DDD` are the three most significant
#'  digits of the number, and `C` an abbreviation which summarises its magnitude.
#'  For instance, `1e6` will be mapped to `1M`, and `123845` to `123k`. Note
#'  that numbers are _truncated_, and not rounded, to obtain `DDD`.
#'
#' @examples
#'
#'  abbrv_large(c(123456, 28475777, 125, 1.3, 0.1, 1))
#'
#' @export
abbrv_large <- function(x) {
  x_str <- rep(NA_character_, length(x))
  for (i in 1:nrow(RANGES)) {
    x_str <- coalesce(
      abbrv_range(x, RANGES[i,]$magnitude, RANGES[i,]$unit),
      x_str
    )
  }
  mag_floor <- min(RANGES$magnitude)
  if_else(abs(x) < mag_floor, as.character(trunc(x)), x_str)
}

abbrv_range <- function(x, magnitude, unit) {
  if_else(abs(x) >= magnitude & abs(x) < magnitude * 1000,
          sprintf('%d%s', trunc(x / magnitude), unit), NA)
}

RANGES <- data.frame(
  magnitude = c(1e9, 1e6, 1e3),
  unit = c('B', 'M', 'k')
)
