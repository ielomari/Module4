convert_temperature <- function(temp, unit, round_result = TRUE) {
  if (!is.numeric(temp)) stop("temp should be a number.")
  
  unit <- toupper(unit)
  
  result <- switch(unit,
                   "C" = temp,
                   "F" = (temp - 32) * 5 / 9,
                   "K" = temp - 273.15,
                   stop("unit should be 'C', 'F' ou 'K'.")
  )
  
  if (round_result) result <- round(result, 2)
  
  return(result)
}
