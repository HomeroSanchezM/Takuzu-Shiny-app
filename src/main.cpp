#include <iostream>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
int add(int x, int y, int z) {
    int sum = x + y + z;
    return sum;
}

// [[Rcpp::export]]
std::vector<std::vector<int>>  init_grille (int size) {
    //int matrice[size][size];
    std::vector<std::vector<int>> matrice(size, std::vector<int>(size));
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            matrice[i][j] = rand() % 2;
            std::cout << matrice[i][j] << " ";
        }
        std::cout << std::endl;
    }
    return matrice;
}

//verification des colones
std::vector<std::vector<int>>  verif_regle_1 (std::vector<std::vector<int>> matrix) {
    int j = 0 ;
    for (int i = 0; i < matrix.size(); i++) {
        if (i>2) {
            if (matrix[i][j]+matrix[i-1][j]+matrix[i-2][j] == 0) {
                matrix[i][j] = 1;
            }
            if (matrix[i][j]+matrix[i-1][j]+matrix[i-2][j] == 3) {
                matrix[i][j] = 0;
            }
        }
    }
}
