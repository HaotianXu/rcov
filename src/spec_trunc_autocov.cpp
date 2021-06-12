//trunc_mean.cpp
#include <RcppArmadillo.h>
#include "spec_trunc_autocov.h"

// [[Rcpp::export]]
arma::mat rcpp_spec_trunc_autocov(const arma::mat& X, int l, double tau) {
  int p = X.n_rows;
  int n = X.n_cols;
  arma::mat op = arma::zeros<arma::mat>(p,p);
  double r;
  arma::mat Y = arma::zeros<arma::mat>(p,p);
  arma::mat result = arma::zeros<arma::mat>(p,p);
  if (l == 0){
    arma::vec v = arma::zeros<arma::vec>(p);
    for (int i = 0; i < n; ++i){
      v = X.col(i);
      op = v * v.t();
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
    arma::vec v1 = arma::zeros<arma::vec>(p);
    arma::vec v2 = arma::zeros<arma::vec>(p);
    for (int i = 0; i < n-l; ++i){
      v1 = X.col(i);
      v2 = X.col(i+l);
      op = v1 * v2.t();
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
