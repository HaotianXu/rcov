#ifndef SPEC_BLOCK_CV_H
#define SPEC_BLOCK_CV_H

arma::vec rcpp_spec_block_cv(const arma::mat& X, int S, int h, int lag, const arma::vec& tau_vec);

#endif
