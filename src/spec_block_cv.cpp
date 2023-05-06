//trunc_mean.cpp
#include <RcppArmadillo.h>
#include "spec_trunc_autocov.h"
#include "spec_block_cv.h"
#include "block_cv.h"
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec rcpp_spec_block_cv(const arma::mat& X, int S, int h, int lag, const arma::vec& tau_vec) {
  const int p = X.n_rows;
  const int n = X.n_cols;
  const int ll = tau_vec.size();
  const int m = std::floor((n-lag)/S);
  double r;
  arma::mat err(ll,2*S,arma::fill::zeros);
  arma::mat op_temp(p,p,arma::fill::zeros);
  arma::mat op1(p,p,arma::fill::zeros);
  arma::uvec idx_test(m, arma::fill::zeros);
  arma::uvec idx_train;
  arma::ivec idx_ending = sample_int(m-1, n-lag-1, S);
  for(int i = 0; i < S; ++i){
    idx_test = arma::linspace<arma::uvec>(i*m, (i+1)*m-1, m);
    op1 = X.cols(idx_test) * (X.cols(idx_test+lag)).t()/m;
    if((i-h)*m <= 0){
      idx_train = arma::linspace<arma::uvec>((i+h+1)*m, n-lag-1, n-lag-(i+h+1)*m);
    }else if((i+h+1)*m-1 >= n-lag-1){
      idx_train = arma::linspace<arma::uvec>(0, (i-h)*m-1, (i-h)*m);
    }else{
      idx_train = join_cols(arma::linspace<arma::uvec>(0, (i-h)*m-1, (i-h)*m), arma::linspace<arma::uvec>((i+h+1)*m, n-lag-1, n-lag-(i+h+1)*m));
    }
    int train_size = idx_train.size();
    for(int jj = 0; jj < ll; ++jj){
      arma::mat op2 = arma::zeros<arma::mat>(p,p);
      for(int k = 0; k < train_size; ++k){
        op_temp = X.col(idx_train(k)) * (X.col(idx_train(k)+lag)).t();
        r = arma::norm(op_temp, 2);
        if(r > tau_vec(jj)){
          op_temp = tau_vec(jj)/r*op_temp;
        }
        op2 = op2 + op_temp;
      }
      err(jj,i) = arma::norm(op1 - op2/train_size, "fro");
    }
  }
  for(int ii = 0; ii < S; ++ii){
    idx_test = arma::linspace<arma::uvec>((idx_ending(ii)-m+1), idx_ending(ii), m);
    op1 = X.cols(idx_test) * (X.cols(idx_test+lag)).t()/m;
    if(idx_ending(ii)-(h+1)*m+1 <= 0){
      idx_train = arma::linspace<arma::uvec>(idx_ending(ii)+h*m+1, n-lag-1, n-lag-idx_ending(ii)-h*m-1);
    }else if(idx_ending(ii)+h*m >= n-lag-1){
      idx_train = arma::linspace<arma::uvec>(0, idx_ending(ii)-(h+1)*m, idx_ending(ii)-(h+1)*m+1);
    }else{
      idx_train = join_cols(arma::linspace<arma::uvec>(idx_ending(ii)+h*m+1, n-lag-1, n-lag-idx_ending(ii)-h*m-1), arma::linspace<arma::uvec>(0, idx_ending(ii)-(h+1)*m, idx_ending(ii)-(h+1)*m+1));
    }
    int train_size = idx_train.size();
    for(int jj = 0; jj < ll; ++jj){
      arma::mat op2 = arma::zeros<arma::mat>(p,p);
      for(int k = 0; k < train_size; ++k){
        op_temp = X.col(idx_train(k)) * (X.col(idx_train(k)+lag)).t();
        r = arma::norm(op_temp, 2);
        if(r > tau_vec(jj)){
          op_temp = tau_vec(jj)/r*op_temp;
        }
        op2 = op2 + op_temp;
      }
      err(jj,ii+S) = arma::norm(op1 - op2/train_size, "fro");
    }
  }
  return(mean(err, 1));
}
