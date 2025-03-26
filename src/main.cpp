#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <random>

using namespace Rcpp;

void testL_V(int t, int i, IntegerMatrix& M) {
  int size = M.nrow() - 1;
  if (t > 2) {
    IntegerVector col = M(_, i);
    IntegerVector sub_col = col[Range(t-2, t-1)];
    if (sum(sub_col) == 0) {
      M(t, i) = 1;
    }
    if (t < size && !IntegerVector::is_na(M(t+1, i))) {
      IntegerVector neighbors = IntegerVector::create(M(t-1, i), M(t+1, i));
      if (sum(neighbors) == 0) {
        M(t, i) = 1;
      }
      if (sum(neighbors) == 2) {
        M(t, i) = 0;
      }
    }
    if (sum(sub_col) == 2) {
      M(t, i) = 0;
    }
  }
}

void testL_H(int t, int i, IntegerMatrix& M) {
  int size = M.ncol() - 1;
  if (i > 2) {
    IntegerVector row = M(t, _);
    IntegerVector sub_row = row[Range(i-2, i-1)];
    if (sum(sub_row) == 0) {
      M(t, i) = 1;
    }
    if (i < size && !IntegerVector::is_na(M(t, i+1))) {
      IntegerVector neighbors = IntegerVector::create(M(t, i-1), M(t, i+1));
      if (sum(neighbors) == 0) {
        M(t, i) = 1;
      }
      if (sum(neighbors) == 2) {
        M(t, i) = 0;
      }
    }
    if (sum(sub_row) == 2) {
      M(t, i) = 0;
    }
  }
}

int fill(IntegerVector V, int size) {
  int n1 = sum(V == 1);
  int n0 = sum(V == 0);
  if (n1 >= size / 2) return 0;
  if (n0 >= size / 2) return 1;
  IntegerVector ref;
  for (int i = 0; i < (size / 2 - n1); ++i) ref.push_back(1);
  for (int i = 0; i < (size / 2 - n0); ++i) ref.push_back(0);
  std::random_shuffle(ref.begin(), ref.end());
  return ref[0];
}

void End_testL(int j, IntegerMatrix& M, int size) {
  if (sum(M(j, _)) != size / 2) {
    for (int i = j; i <= size; ++i) M(j, i) = NA_INTEGER;
    ligne(j, 0, M, size);
  }
}

void End_testC(int j, IntegerMatrix& M, int size) {
  if (sum(M(_, j)) != size / 2) {
    for (int i = j + 1; i <= size; ++i) M(i, j) = NA_INTEGER;
    colonne(j, 0, M, size);
  }
}

void ligne(int x, int a, IntegerMatrix& M, int size) {
  if (x > 2) {
    for (int i = x + a; i <= size; ++i) {
      testL_V(x, i, M);
    }
  }
  for (int i = x + a; i <= size; ++i) {
    if (i <= 2) {
      M(x, i) = fill(M(x, _), size);
    } else {
      if (IntegerVector::is_na(M(x, i))) {
        testL_H(x, i, M);
        if (IntegerVector::is_na(M(x, i))) {
          M(x, i) = fill(M(x, _), size);
        }
      }
    }
  }
}

void colonne(int x, int a, IntegerMatrix& M, int size) {
  if (x > 2) {
    for (int i = x + 1 + a; i <= size; ++i) {
      testL_H(i, x, M);
    }
  }
  for (int i = x + 1 + a; i <= size; ++i) {
    if (i <= 2) {
      M(i, x) = fill(M(_, x), size);
    } else {
      if (IntegerVector::is_na(M(i, x))) {
        testL_V(i, x, M);
        if (IntegerVector::is_na(M(i, x))) {
          M(i, x) = fill(M(_, x), size);
        }
      }
    }
  }
}

// [[Rcpp::export]]
IntegerMatrix generateGrid(int size) {
  IntegerMatrix M(size + 1, size + 1);
  std::fill(M.begin(), M.end(), NA_INTEGER);
  for (int j = 1; j <= size; ++j) {
    if (j <= size - 1) {
      ligne(j, 0, M, size);
      End_testL(j, M, size);
      colonne(j, 0, M, size);
      End_testC(j, M, size);
    } else {
      ligne(j, 0, M, size);
    }
  }
  return M(Range(0, size - 1), Range(0, size - 1));
}