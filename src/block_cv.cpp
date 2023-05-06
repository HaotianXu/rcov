//block_cv.cpp
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <algorithm>
#include <random>
#include "block_cv.h"
#include "trunc_mean.h"
#include "huber_mean.h"
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::IntegerVector sample_int(int B, int E, int S) {
  Rcpp::IntegerVector pool = Rcpp::seq(B, E);
  std::random_device rd;
  std::mt19937 g(rd());
  std::shuffle(pool.begin(), pool.end(), g);
  return pool[Rcpp::Range(0, S-1)];
}

// [[Rcpp::export]]
arma::vec rcpp_block_cv(const arma::vec& x, int S, int h, const arma::vec& tau_vec, unsigned int M_est) {
  const int n = x.size();
  const int m = n/S;
  const int l = tau_vec.size();
  arma::vec err_cv(l, arma::fill::zeros);
  arma::uvec idx = arma::linspace<arma::uvec>(0, n-1, n);
  arma::ivec idx_ending = sample_int(m-1, n-1, S);
  arma::vec err(2*S, arma::fill::zeros);
  arma::vec x_test(m, arma::fill::zeros);
  arma::vec diff_test(m, arma::fill::zeros);
  arma::uvec idx_train;
  if(M_est == 0){
    for(int j = 0; j < l; ++j){
      for(int i = 0; i < S; ++i){
        x_test = x(arma::linspace<arma::uvec>(i*m, (i+1)*m-1, m));
        if((i-h)*m <= 0){
          idx_train = arma::linspace<arma::uvec>((i+h+1)*m, n-1, n-(i+h+1)*m);
        }else if((i+h+1)*m-1 >= n-1){
          idx_train = arma::linspace<arma::uvec>(0, (i-h)*m-1, (i-h)*m);
        }else{
          idx_train = join_cols(arma::linspace<arma::uvec>(0, (i-h)*m-1, (i-h)*m), arma::linspace<arma::uvec>((i+h+1)*m, n-1, n-(i+h+1)*m));
        }
        arma::vec x_train = x(idx_train);
        diff_test = x_test - rcpp_trunc_mean(x_train, tau_vec(j));
        err(i) = arma::accu(arma::abs(diff_test));
      }
      for(int r = 0; r < S; ++r){
        x_test = x(arma::linspace<arma::uvec>((idx_ending(r)-m+1), idx_ending(r), m));
        if(idx_ending(r)-(h+1)*m+1 <= 0){
          idx_train = arma::linspace<arma::uvec>(idx_ending(r)+h*m+1, n-1, n-idx_ending(r)-h*m-1);
        }else if(idx_ending(r)+h*m >= n-1){
          idx_train = arma::linspace<arma::uvec>(0, idx_ending(r)-(h+1)*m, idx_ending(r)-(h+1)*m+1);
        }else{
          idx_train = join_cols(arma::linspace<arma::uvec>(idx_ending(r)+h*m+1, n-1, n-idx_ending(r)-h*m-1), arma::linspace<arma::uvec>(0, idx_ending(r)-(h+1)*m, idx_ending(r)-(h+1)*m+1));
        }
        arma::vec x_train = x(idx_train);
        diff_test = x_test - rcpp_trunc_mean(x_train, tau_vec(j));
        err(r+S) = arma::accu(arma::abs(diff_test));
      }
      err_cv(j) = mean(err);
    }
  }else{
    for(int j = 0; j < l; ++j){
      for(int i = 0; i < S; ++i){
        x_test = x(arma::linspace<arma::uvec>(i*m, (i+1)*m-1, m));
        if((i-h)*m <= 0){
          idx_train = arma::linspace<arma::uvec>((i+h+1)*m, n-1, n-(i+h+1)*m);
        }else if((i+h+1)*m-1 >= n-1){
          idx_train = arma::linspace<arma::uvec>(0, (i-h)*m-1, (i-h)*m);
        }else{
          idx_train = join_cols(arma::linspace<arma::uvec>(0, (i-h)*m-1, (i-h)*m), arma::linspace<arma::uvec>((i+h+1)*m, n-1, n-(i+h+1)*m));
        }
        arma::vec x_train = x(idx_train);
        diff_test = x_test - rcpp_huber_mean(x_train, tau_vec(j));
        err(i) = arma::accu(arma::abs(diff_test));
      }
      for(int r = 0; r < S; ++r){
        x_test = x(arma::linspace<arma::uvec>((idx_ending(r)-m+1), idx_ending(r), m));
        if(idx_ending(r)-(h+1)*m+1 <= 0){
          idx_train = arma::linspace<arma::uvec>(idx_ending(r)+h*m+1, n-1, n-idx_ending(r)-h*m-1);
        }else if(idx_ending(r)+h*m >= n-1){
          idx_train = arma::linspace<arma::uvec>(0, idx_ending(r)-(h+1)*m, idx_ending(r)-(h+1)*m+1);
        }else{
          idx_train = join_cols(arma::linspace<arma::uvec>(idx_ending(r)+h*m+1, n-1, n-idx_ending(r)-h*m-1), arma::linspace<arma::uvec>(0, idx_ending(r)-(h+1)*m, idx_ending(r)-(h+1)*m+1));
        }
        arma::vec x_train = x(idx_train);
        diff_test = x_test - rcpp_huber_mean(x_train, tau_vec(j));
        err(r+S) = arma::accu(arma::abs(diff_test));
      }
      err_cv(j) = mean(err);
    }
  }
  return(err_cv);
}
