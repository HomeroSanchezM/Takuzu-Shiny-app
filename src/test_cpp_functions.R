library(Rcpp)

sourceCpp("src/main.cpp")

M <- generateGrid(10)  # Génère une grille 10x10
print(M)