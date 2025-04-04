recommend_books <- function(skill, random = FALSE, data_path = "book_recommendations.csv") {
  books <- read.csv(data_path, stringsAsFactors = FALSE)
  
  matched <- books[books$skill_id == skill, ]
  
  if (nrow(matched) > 0) {
    return(head(matched[, c("title", "author")], 3))
  } else {
    message("🔍 Aucun livre trouvé pour cette compétence.")
    if (random) {
      message("🎲 Sélection de livres au hasard...")
      return(head(books[sample(nrow(books)), c("title", "author")], 3))
    } else {
      return(NULL)
    }
  }
}
