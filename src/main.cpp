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
    std::cout << std::endl;
    return matrice;
}

//verification des ligne en premier
// [[Rcpp::export]]
std::vector<std::vector<int>>  verif_regle_1 (std::vector<std::vector<int>> matrix) {
    int j = 0 ;
    for (int i = 0; i < matrix.size(); i++) {
        if (i>1) {
            //std::cout <<"On rentre dans le if(j>2) pour la colone"<<i<<std::endl;
            if (matrix[i][j]+matrix[i-1][j]+matrix[i-2][j] == 0) {
                matrix[i][j] = 1;
            }
            if (matrix[i][j]+matrix[i-1][j]+matrix[i-2][j] == 3) {
                matrix[i][j] = 0;
            }
        }
        for (int j = 0; j < matrix[i].size(); j++) {
            if (j>1) {
                //std::cout <<"On rentre dans le if(j>2) pour la ligne"<<j<<std::endl;
                if (matrix[i][j]+matrix[i][j-1]+matrix[i][j-2] == 0) {
                    matrix[i][j] = 1;
                }
                if (matrix[i][j]+matrix[i][j-1]+matrix[i][j-2] == 3) {
                    matrix[i][j] = 0;
                }
            }

        }
    }
    return matrix;
}

//verification des colone en premier
// [[Rcpp::export]]
std::vector<std::vector<int>>  verif_regle_1_version2 (std::vector<std::vector<int>> matrix) {
    int i = 0 ;
    for (int j = 0; j < matrix.size(); j++) {
        if (j>1) {
            //std::cout <<"On rentre dans le if(j>2) pour la colone"<<i<<std::endl;
            if (matrix[i][j]+matrix[i][j-1]+matrix[i][j-2] == 0) {
                matrix[i][j] = 1;
            }
            if (matrix[i][j]+matrix[i][j-1]+matrix[i][j-1] == 3) {
                matrix[i][j] = 0;
            }
        }
        for (int i = 0; i < matrix.size(); i++) {
            if (i>1) {
                //std::cout <<"On rentre dans le if(j>2) pour la ligne"<<j<<std::endl;
                if (matrix[i][j]+matrix[i-1][j]+matrix[i-1][j] == 0) {
                    matrix[i][j] = 1;
                }
                if (matrix[i][j]+matrix[i-1][j]+matrix[i-2][j] == 3) {
                    matrix[i][j] = 0;
                }
            }

        }
    }
    return matrix;
}

/*
std::vector<std::vector<int>>  verif_regle_1_colones (std::vector<std::vector<int>> matrix, int j) {
    for (int i = 0; i < matrix.size(); i++) {
        if (i>2) {
            if (matrix[i][j]+matrix[i-1][j]+matrix[i-2][j] == 0) {
                matrix[i][j] = 1;
                verif_regle_1_ligne(matrix, i);
            }
            if (matrix[i][j]+matrix[i-1][j]+matrix[i-2][j] == 3) {
                matrix[i][j] = 0;
                verif_regle_1_ligne(matrix, i);
            }
        }

}
*/

// [[Rcpp::export]]
void display_matrix(std::vector<std::vector<int>> matrix) {
    for (int i = 0; i < matrix.size(); i++) {
        for (int j = 0; j < matrix[i].size(); j++) {
            std::cout << matrix[i][j] << " ";
        }
        std::cout << std::endl;
    }
}

