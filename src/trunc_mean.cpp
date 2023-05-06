//trunc_mean.cpp
#include <RcppArmadillo.h>
#include "trunc_mean.h"

// [[Rcpp::export]]
double rcpp_trunc_mean(const arma::vec& x, double tau) {
  const int n = x.size();
  arma::vec tmp(n);
  for(int i = 0; i < n; ++i){
    if(x[i] >= tau){
      tmp[i] = tau;
    } else if(x[i] <= -tau){
      tmp[i] = -tau;
    } else {
      tmp[i] = x[i];
    }
  }
  return arma::accu(tmp) / n;
}
