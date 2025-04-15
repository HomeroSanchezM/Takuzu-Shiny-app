#' Génère une grille de Takuzu valide
#'
#' @param size Taille de la grille (doit être un nombre pair, par défaut 10)
#' @return Une matrice carrée représentant la grille solution
#' @export
generate_takuzu_grid <- function(size = 10) {
  # Vérification que la taille est paire
  if (size %% 2 != 0) {
    stop("La taille de la grille doit être un nombre pair")
  }
  
  # Initialisation de la matrice avec une ligne et colonne supplémentaires
  M <- matrix(
    data = NA,
    nrow = size + 1,
    ncol = size + 1,
    byrow = TRUE,
    dimnames = NULL
  )
  
  # Fonction pour vérifier les contraintes verticales
  testL_V <- function(t, i, M) {
    if (sum(M[(t-2):(t-1), i], na.rm = TRUE) == 0) M[t, i] <- 1
    if (!is.na(M[t+1, i])) {
      if (sum(M[c(t-1, t+1), i], na.rm = TRUE) == 0) M[t, i] <- 1
      if (sum(M[c(t-1, t+1), i], na.rm = TRUE) == 2) M[t, i] <- 0
    }
    if (sum(M[(t-2):(t-1), i], na.rm = TRUE) == 2) M[t, i] <- 0
    return(M)
  }
  
  # Fonction pour vérifier les contraintes horizontales
  testL_H <- function(t, i, M) {
    if (sum(M[t, (i-2):(i-1)], na.rm = TRUE) == 0) M[t, i] <- 1
    if (!is.na(M[t, i+1])) {
      if (sum(M[t, c(i-1, i+1)], na.rm = TRUE) == 0) M[t, i] <- 1
      if (sum(M[t, c(i-1, i+1)], na.rm = TRUE) == 2) M[t, i] <- 0
    }
    if (sum(M[t, (i-2):(i-1)], na.rm = TRUE) == 2) M[t, i] <- 0
    return(M)
  }
  
  # Fonction pour remplir une case selon les contraintes
  fill <- function(V, size) {
    n1 <- sum(V == 1, na.rm = TRUE)
    n0 <- sum(V == 0, na.rm = TRUE)
    if (n1 > size/2) return(0)
    if (n0 > size/2) return(1)
    ref <- c(rep(1, (size/2) - n1), rep(0, (size/2) - n0))
    if (length(ref) == 0) return(NA)
    ref <- sample(ref, size = length(ref), replace = FALSE)
    return(sample(ref, size = 1, replace = FALSE))
  }
  
  # Fonction pour remplir une ligne
  ligne <- function(x, a, M, size) {
    if (x > 2) {
      for (i in seq(from = x + a, to = size, by = 1)) {
        M <- testL_V(x, i, M)
      }
    }
    
    for (i in seq(from = x + a, to = size, by = 1)) {
      if (i <= 2) {
        v <- M[x, ]
        M[x, i] <- fill(v, size)
      }
      if (i > 2) {
        if (is.na(M[x, i])) {
          M <- testL_H(x, i, M)
          if (is.na(M[x, i])) {
            v <- M[x, ]
            M[x, i] <- fill(v, size)
          }
        }
      }
    }
    return(M)
  }
  
  # Fonction pour remplir une colonne
  colonne <- function(x, a, M, size) {
    if (x > 2) {
      for (i in seq(from = x + 1 + a, to = size, by = 1)) {
        M <- testL_H(i, x, M)
      }
    }
    
    for (i in seq(from = x + 1 + a, to = size, by = 1)) {
      if (i <= 2) {
        v <- M[, x]
        M[i, x] <- fill(v, size)
      }
      if (i > 2) {
        if (is.na(M[i, x])) {
          M <- testL_V(i, x, M)
          if (is.na(M[i, x])) {
            v <- M[, x]
            M[i, x] <- fill(v, size)
          }
        }
      }
    }
    return(M)
  }
  
  # Génération de la grille
  for (j in 1:size) {
    if (j <= size-1) {
      M <- ligne(j, 0, M, size)
      M <- colonne(j, 0, M, size)
    } else {
      M <- ligne(j, 0, M, size)
    }
  }
  
  # Retourne la grille sans la ligne et colonne supplémentaires
  return(M[1:size, 1:size])
}