#' @export
rotate_x_text <- function(angle = 45)
  theme(axis.text.x = element_text(angle = angle, hjust = 1))

#' @export
rotate_y_text <- function(angle = 45) theme(
  axis.text.y = element_text(angle = angle, hjust = 1))

#' @export
legend_bottom <- function() theme(legend.position="bottom")

if_else <- function(logical, true, false) {
  res <- rep(NA, length(logical))
  lna <- !is.na(logical)
  res[lna & logical] <- true[lna & logical]
  res[lna & !logical] <- false[lna & !logical]
  res
}

coalesce <- function(x1, x2) {
  x1[is.na(x1)] <- x2[is.na(x1)]
  x1
}

truncate <- function(x, decimals = 0) {
  trunc(x * 10^decimals) / 10^decimals
}
