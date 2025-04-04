describe_vector <- function(x, summary_fun = NULL) {
  if (!is.numeric(x)) stop("x doit être un vecteur numérique.")
  
  if (is.null(summary_fun)) {
    return(list(
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE)
    ))
  }
  
  summary_fun <- tolower(summary_fun)
  
  if (summary_fun == "mean") {
    return(mean(x, na.rm = TRUE))
  } else if (summary_fun == "median") {
    return(median(x, na.rm = TRUE))
  } else if (summary_fun == "sd") {
    return(sd(x, na.rm = TRUE))
  } else {
    stop("summary_fun should be NULL, 'mean', 'median' ou 'sd'.")
  }
}
