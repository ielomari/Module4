df <- read.csv("cleaning_exercise_full.csv")

clean_and_merge_vars <- function(df, drop_original = TRUE) {
  if (!is.data.frame(df)) stop("L'objet fourni n'est pas un data frame.")
  
  major_cols <- grep("_major$", names(df), value = TRUE)
  minor_cols <- grep("_minor$", names(df), value = TRUE)

  var_names_major <- sub("_major$", "", major_cols)
  var_names_minor <- sub("_minor$", "", minor_cols)

  if (!all(var_names_major %in% var_names_minor)) {
    stop("Il y a des colonnes _major sans correspondance _minor.")
  }
  
  var_names <- intersect(var_names_major, var_names_minor)

  for (var in var_names) {
    major_col <- paste0(var, "_major")
    minor_col <- paste0(var, "_minor")
    
    if (!major_col %in% names(df) || !minor_col %in% names(df)) {
      warning(paste("Colonnes manquantes pour", var))
      next
    }

    df[[var]] <- df[[major_col]] + df[[minor_col]]

    if (drop_original) {
      df[[major_col]] <- NULL
      df[[minor_col]] <- NULL
    }
  }
    ordered_vars <- paste0("var", 1:26)
    vars_to_keep <- ordered_vars[ordered_vars %in% names(df)]
    df <- df[ , vars_to_keep, drop = FALSE]
    
    
  return(df)
}

df_clean <- clean_and_merge_vars(df, drop_original = TRUE)

head(df_clean)
