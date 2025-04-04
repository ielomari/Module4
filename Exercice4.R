complex_sum <- function(x) {
  x <- x[!is.na(x) & x >= 0]
  sum(x)
}
