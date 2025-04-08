apply_and_sum <- function(x, fun) {
  if (!is.numeric(x)) stop("x doit être un vecteur numérique.")
  if (!is.function(fun)) stop("Le deuxième argument doit être une fonction.")
  
  results <- sapply(x, fun)
  sum(results)
}

