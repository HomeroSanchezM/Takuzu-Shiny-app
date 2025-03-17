library(Rcpp)

sourceCpp("src/main.cpp")

add(1,2,3)


display_matrix(verif_regle_1(init_grille(6)))

display_matrix(verif_regle_1_version2(verif_regle_1(init_grille(6))))
