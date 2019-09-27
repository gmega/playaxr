RANGES <- data.frame(
  magnitude = c(1e9, 1e6, 1e3),
  unit = c('B', 'M', 'k')
)

if_else <- function(logical, true, false) {
  res <- rep(NA, length(logical))
  res[logical] <- true[logical]
  res[!logical] <- false[!logical]
  res
}

coalesce <- function(x1, x2) {
  x1[is.na(x1)] <- x2[is.na(x1)]
  x1
}
