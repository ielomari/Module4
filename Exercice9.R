make_aggregator <- function(name) {
  if (!name %in% c("sum", "mean", "max")) {
    stop("La fonction doit être 'sum', 'mean' ou 'max'.")
  }

  if (name == "sum") {
    return(function(x) sum(x, na.rm = TRUE))
  } else if (name == "mean") {
    return(function(x) mean(x, na.rm = TRUE))
  } else if (name == "max") {
    return(function(x) max(x, na.rm = TRUE))
  }
}
