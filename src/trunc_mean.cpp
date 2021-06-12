//trunc_mean.cpp
#include <RcppArmadillo.h>
#include "trunc_mean.h"

// [[Rcpp::export]]
double rcpp_trunc_mean(const arma::vec& x, double tau) {
  int n = x.size();
  double total = 0;
  double y = 0;
  for(int i = 0; i < n; ++i){
    if ((x[i] < tau) & (x[i] > -tau)){
      y = x[i];
    }else if(x[i] >= tau){
      y = tau;
    }else{
      y = -tau;
    }
    total += y;
  }
  return total/n;
}
