//trunc_mean.cpp
#include <RcppArmadillo.h>
#include "spec_trunc_autocov.h"

// [[Rcpp::export]]
arma::mat rcpp_spec_trunc_autocov(const arma::mat& X, int l, double tau) {
  const int p = X.n_rows;
  const int n = X.n_cols;
  arma::mat op(p,p,arma::fill::zeros);
  arma::mat Y(p,p,arma::fill::zeros);
  arma::mat result(p,p,arma::fill::zeros);
  double r;
  if (l == 0){
    for (int i = 0; i < n; ++i){
      op = X.col(i) * X.col(i).t();
      r = arma::norm(op, 2);
      if (r < tau){
        Y = op;
      }else{
        Y = tau/r * op;
      }
      result += Y;
    }
    result = result/n;
  }else if (l > 0){
    for (int i = 0; i < n-l; ++i){
      op = X.col(i) * X.col(i+l).t();
      r = arma::norm(op, 2);
      if (r < tau){
        Y = op;
      }else{
        Y = tau/r * op;
      }
      result += Y;
    }
    result = result/(n-l);
  }
  return result;
}
