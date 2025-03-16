#include <iostream>
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int add(int x, int y, int z) {
    int sum = x + y + z;
    return sum;
}


