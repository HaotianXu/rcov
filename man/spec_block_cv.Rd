% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean.R
\name{spec_block_cv}
\alias{spec_block_cv}
\title{Block-wise cross-validation to select the spectrum-wise robustification parameter tau for autocovariance}
\usage{
spec_block_cv(X, S, h, lag, tau.vec, ...)
}
\arguments{
\item{X}{A pxn matrix of observated multivariate time series.}

\item{S}{A strictly positive integer corresponding to the chosen number of non-overlapping blocks.}

\item{h}{A strictly positive integer corresponding to how many neighboring blocks will be removed.}

\item{lag}{A strictly non-negative integer corresponding to the lag of the autocovariance.}

\item{tau.vec}{A \code{numeric} vector of candidate robustification parameters (positive values).}

\item{...}{Additional arguments.}
}
\value{
A \code{numeric} vector of cross validation errors (in l2 norm), corresponding to the tau.vec.
}
\description{
As the title
}
\examples{
n = 100
p = 50
Sigma = diag(1, p)
set.seed(123)
X = VAR1_simu(n = n, mu = rep(0, p), skip = 300, Sigma.mat = Sigma, rho = 0.7, err.dist = "pareto")
tau.vec = seq(from = 10, to = 100, length.out = 10)
spec_block_cv(X, S = 10, h = 1, lag = 1, tau.vec)

}
\author{
Haotian Xu and Stephane Guerrier
}
