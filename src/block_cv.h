#ifndef BLOCK_CV_H
#define BLOCK_CV_H

Rcpp::IntegerVector sample_int(int B, int E, int S);
arma::vec rcpp_block_cv(const arma::vec& x, int S, int h, const arma::vec& tau_vec, unsigned int M_est);

#endif
