safe_divide <- function(a, b) {
  tryCatch({
    if (!is.numeric(a) || !is.numeric(b)) stop("Argument not numeric. Invalid Input")
    if (b == 0) stop("Cannot divide by zero.")
    return(a / b)
  }, error = function(e) {
    return(e$message)
  })
}