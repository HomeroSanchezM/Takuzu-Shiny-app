#' @import shiny
NULL

#' Lance le jeu Takuzu
#'
#' Cette fonction charge les dépendances nécessaires et lance l'interface Shiny.
#' @export
play_takuzu <- function() {
  source("takuzu_generation.R")
  source("takuzu_shiny.R")
  play_takuzu()
}