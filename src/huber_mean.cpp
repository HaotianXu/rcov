//trunc_mean.cpp
#include <RcppArmadillo.h>
#include "huber_mean.h"

// [[Rcpp::export]]
double rcpp_huber_mean(const arma::vec& x, double tau) {
  double eps = 1e-8;
  const int n = x.size();
  double mu_new = arma::mean(x);
  double mu_old = 0;
  arma::vec r;
  arma::vec b(n, arma::fill::ones);
  arma::vec c(n, arma::fill::zeros);
  while (std::abs(mu_new - mu_old) > eps){
    mu_old = mu_new;
    r = x - mu_new;
    for(int i = 0; i < n; ++i){
      if(std::abs(r[i]) > tau){
        b[i] = tau/std::abs(r[i]);
      }
      c[i] = x[i]*b[i];
    }
    mu_new = arma::accu(c)/arma::accu(b);
  }
  return mu_new;
}
