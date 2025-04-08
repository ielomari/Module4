flexible_plot <- function(x, y, ...) {
  if (!is.numeric(x) || !is.numeric(y)) stop("x et y should be numeric.")
  
  plot(x, y, ...)
}
