#' @export
trunc_mean <- function(x, tau) {
  .Call('_rcov_rcpp_trunc_mean', PACKAGE = 'rcov', x, tau)
}

#' @export
huber_mean <- function(x, tau) {
  .Call('_rcov_rcpp_huber_mean', PACKAGE = 'rcov', x, tau)
}

#' @export
spec_trunc_autocov <- function(X, l, tau) {
  .Call('_rcov_rcpp_spec_trunc_autocov', PACKAGE = 'rcov', X, l, tau)
}

#' @export
block_cv <- function(x, S, h, tau_vec, M_est) {
  .Call('_rcov_rcpp_block_cv', PACKAGE = 'rcov', x, S, h, tau_vec, M_est)
}

#' @export
spec_block_cv <- function(X, S, h, lag, tau_vec) {
  .Call('_rcov_rcpp_spec_block_cv', PACKAGE = 'rcov', X, S, h, lag, tau_vec)
}


#' @title Element-wise truncated mean estimator
#' @description Computes the element-wise truncated mean estimator
#' @param x         A \code{numeric} vector of observations.
#' @param tau       A \code{numeric} scalar corresponding to the truncation parameter (assumed to be larger than 0)
#' @param ...      Additional arguments.
#' @return A \code{numeric} corresponding to the truncated mean.
#' @export
#' @author Haotian Xu
#' @examples
#' # TO DO
Trunc_mean <- function(x, tau, ...){
  # Add check on tau
  if (tau <= 0){
    stop("Tau should be larger than 0.")
  }
  mean(sapply(x, function(y){ifelse(abs(y)>tau, sign(y)*tau, y)}))
}

#' @title Interlacing median-of-means estimator (with even number of blocks)
#' @description Compute the interlacing median-of-means estimator
#' @param x         A \code{numeric} vector of observations.
#' @param K         A strictly positive integer corresponding to the interlacing block length (assumed to be larger than 0)
#' @param ...      Additional arguments.
#' @return A \code{list} with the structure:
#' \itemize{
#'  \item mean_median    The final estimator is aggregated by averaging the median-of-means for odd-blocks and the median-of-means for even-blocks
#'  \item mom            The final estimator is the sample median of all block means
#'  \item ...            Additional parameters
#' }
#' @export
#' @author Haotian Xu
#' @examples
#' # TO DO
MoM_mean <- function(x, K, ...){

  # Add check on K
  if (K <= 0){
    stop("K should be larger than 0.")
  }

  n <- length(x)
  m <- floor(n/(2*K)) # number of even or odd blocks

  mean_odd <- rep(NA, K)
  mean_even <- rep(NA, K)

  for(i in 1:K){
    mean_odd[i] <- mean(x[((2*i-2)*m+1):((2*i-1)*m)])
    mean_even[i] <- mean(x[((2*i-1)*m+1):(2*i*m)])
  }

  median_odd <- median(mean_odd)
  median_even <- median(mean_even)
  mean_median <- (median_odd+median_even)/2
  median_total <- median(c(mean_odd,mean_odd))

  outlist <- list(mean_median = mean_median, mom = median_total, ...)
  outlist
}

#' @title Classical median-of-means estimator (the number of blocks is not necessary be even integer)
#' @description Compute the classical median-of-means estimator
#' @param x         A \code{numeric} vector of observations.
#' @param K         A strictly positive integer scalar corresponding to the interlacing block length (assumed to be larger than 0)
#' @param ...      Additional arguments.
#' @return A \code{list} with the structure:
#' \itemize{
#'  \item mean_median    The classical median-of-means estimator
#'  \item ...            Additional parameters
#' }
#' @export
#' @author Haotian Xu
#' @examples
#' # TO DO
MoM_mean2 <- function(x, K, ...){
  # Add check on K
  if (K <= 0){
    stop("K should be larger than 0.")
  }

  n <- length(x)
  m <- floor(n/K) # number of blocks

  mean_block <- rep(NA, K)

  for(i in 1:K){
    mean_block[i] <- mean(x[((i-1)*m+1):(i*m)])
  }

  median_means <- median(mean_block)

  outlist<-list(mom=median_means, ...)
  return(outlist)
}


#' @title Simulating VAR2 models
#' @description Simulate a p dimensional VAR2 models with length n.
#' @param n         A strictly positive integer corresponding to the length of output series.
#' @param mu        A \code{numeric} vector of mean of VAR2 model.
#' @param skip      A strictly positive integer corresponding to the length of initial series not returned.
#' @param A1        A p x p transition matrix for lag-1 term.
#' @param A2        A p x p transition matrix for lag-2 term.
#' @param Gamma.mat A p x p matrix and Gamma.mat^2 is the covariance matrix of the error term.
#' @param err.dist  A \code{string} variable specifying the distribution of the error term, must be one of ("normal", "pareto", "lognorm", "t3").
#' @return A p x n matrix
#' @export
#' @author Haotian Xu
#' @examples
#' NEED TO BE MODIFIED!!!!!!
var2_simu = function(n, mu, skip = 300, A1, A2, Gamma.mat, err.dist = "t3"){
  p = dim(A1)[1]
  X = matrix(NA, nrow = p, ncol = n+skip+2)
  if(err.dist == "normal"){
    X[,1] = Gamma.mat %*% rnorm(p)
    X[,2] = A1 %*% X[,1] + Gamma.mat %*% rnorm(p)
    for(i in 1:(n+skip)){
      X[,i+2] = A1 %*% X[,i+1] + A2 %*% X[,i] + Gamma.mat %*% rnorm(p)
    }
    X.var2 = sweep(X[,(skip+3):(n+skip+2)], MARGIN = 1, mu, "+")
  }else if(err.dist == "pareto"){
    X[,1] = Gamma.mat %*% (2*(rpareto(p, a = 3, b = 1) - 3/2)/sqrt(3))
    X[,2] = A1 %*% X[,1] + Gamma.mat %*% (2*(rpareto(p, a = 3, b = 1) - 3/2)/sqrt(3))
    for(i in 1:(n+skip)){
      X[,i+2] = A1 %*% X[,i+1] + A2 %*% X[,i] + Gamma.mat %*% (2*(rpareto(p, a = 3, b = 1) - 3/2)/sqrt(3))
    }
    X.var2 = sweep(X[,(skip+3):(n+skip+2)], MARGIN = 1, mu, "+")
  }else if(err.dist == "lognorm"){
    X[,1] = Gamma.mat %*% (exp(rnorm(p)) - exp(1/2))/sqrt(exp(2)-exp(1))
    X[,2] = A1 %*% X[,1] + Gamma.mat %*% (exp(rnorm(p)) - exp(1/2))/sqrt(exp(2)-exp(1))
    for(i in 1:(n+skip)){
      X[,i+2] = A1 %*% X[,i+1] + A2 %*% X[,i] + Gamma.mat %*% (exp(rnorm(p)) - exp(1/2))/sqrt(exp(2)-exp(1))
    }
    X.var2 = sweep(X[,(skip+3):(n+skip+2)], MARGIN = 1, mu, "+")
  }else if(startsWith(err.dist, "t") && as.numeric(substring(err.dist, 2)) > 2){
    df.t = as.numeric(substring(err.dist, 2))
    X[,1] = Gamma.mat %*% rt(p,df = df.t)/sqrt(df.t/(df.t - 2))
    X[,2] = A1 %*% X[,1] + Gamma.mat %*% rt(p,df = df.t)/sqrt(df.t/(df.t - 2))
    for(i in 1:(n+skip)){
      X[,i+2] = A1 %*% X[,i+1] + A2 %*% X[,i] + Gamma.mat %*% rt(p,df = df.t)/sqrt(df.t/(df.t - 2))
    }
    X.var2 = sweep(X[,(skip+3):(n+skip+2)], MARGIN = 1, mu, "+")
  }else{
    stop("err.dist should be correctly specified.")
  }
  return(X.var2)
}

#' @title Simulating Linear process model with heteroscedastic errors
#' @description Simulate a p dimensional Linear process model with length n. The setting follows APPENDIX D: SIMULATION STUDY in Zhang&Wu(2017)
#' @param n         A strictly positive integer corresponding to the length of output series.
#' @param mu        A \code{numeric} vector of mean of VAR2 model.
#' @param rho       A scalar in (0,1) indicating the decay rate of the transition matrix for lag-k term as the k grows.
#' @param K         A strictly positive integer and (K-1) represents the number of lags considered.
#' @param M.list    A list of p x p matrices, and (rho^(k-1)*M.list[[k]]) be the transition matrix for lag-k term.
#' @param ...      Additional arguments.
#' @return A p x n matrix
#' @export
#' @author Haotian Xu
#' @examples
#' TO DO
linear_simu = function(n, mu, M.list, rho, K = 1001, err.dist = "t3"){
  p = dim(M.list[[1]])[1]
  eta.mat = matrix(NA, nrow = p, ncol = K+n-1)
  X = matrix(0, nrow = p, ncol = n)
  if(err.dist == "normal"){
    epsilon.mat = matrix(rnorm((K+n)*p), nrow = p, ncol = K+n)
    for(i in 1:p){
      for(j in 1:(K+n-1)){
        eta.mat[i,j] = epsilon.mat[i,j+1]*(0.8*epsilon.mat[i,j]^2 + 0.2)^0.5
      }
    }
    for(i in 1:n){
      for(k in 1:K){
        X[,i] = X[,i] + rho^(k-1) * M.list[[k]] %*% eta.mat[,K+i-1-(k-1)]
      }
    }
  }else if(err.dist == "pareto"){
    epsilon.mat = matrix(2*(rpareto((K+n)*p, a = 3, b = 1) - 3/2)/sqrt(3) , nrow = p, ncol = K+n)
    for(i in 1:p){
      for(j in 1:(K+n-1)){
        eta.mat[i,j] = epsilon.mat[i,j+1]*(0.8*epsilon.mat[i,j]^2 + 0.2)^0.5
      }
    }
    for(i in 1:n){
      for(k in 1:K){
        X[,i] = X[,i] + rho^(k-1) * M.list[[k]] %*% eta.mat[,K+i-1-(k-1)]
      }
    }
  }else if(err.dist == "lognorm"){
    epsilon.mat = matrix((exp(rnorm((K+n)*p)) - exp(1/2))/sqrt(exp(2)-exp(1)), nrow = p, ncol = K+n)
    for(i in 1:p){
      for(j in 1:(K+n-1)){
        eta.mat[i,j] = epsilon.mat[i,j+1]*(0.8*epsilon.mat[i,j]^2 + 0.2)^0.5
      }
    }
    for(i in 1:n){
      for(k in 1:K){
        X[,i] = X[,i] + rho^(k-1) * M.list[[k]] %*% eta.mat[,K+i-1-(k-1)]
      }
    }
  }else if(startsWith(err.dist, "t") && as.numeric(substring(err.dist, 2)) > 2){
    df.t = as.numeric(substring(err.dist, 2))
    epsilon.mat = matrix(rt((K+n)*p, df.t)/sqrt(df.t/(df.t-2)), nrow = p, ncol = K+n)
    for(i in 1:p){
      for(j in 1:(K+n-1)){
        eta.mat[i,j] = epsilon.mat[i,j+1]*(0.8*epsilon.mat[i,j]^2 + 0.2)^0.5
      }
    }
    for(i in 1:n){
      for(k in 1:K){
        X[,i] = X[,i] + rho^(k-1) * M.list[[k]] %*% eta.mat[,K+i-1-(k-1)]
      }
    }
  }
  X = sweep(X, MARGIN = 1, mu, "+")
  return(X)
}



#' @title Simulating VAR1 model with different error distributions
#' @description Simulate a p dimensional VAR1 model with length n. The setting follows Ke et.al.(2019)
#' @param n          A strictly positive integer corresponding to the length of output series.
#' @param skip       A strictly positive integer corresponding to the length of initial series not returned.
#' @param mu         A \code{numeric} vector of mean of VAR1 model.
#' @param rho        A scalar in (0,1) indicating the decay rate of the transition matrix for lag-k term as the k grows.
#' @param Sigma.mat  A p x p matrix and Sigma.mat^2 is the covariance matrix of the error term.
#' @param ...        Additional arguments.
#' @return   A p x n matrix
#' @export
#' @author Haotian Xu
#' @examples
#' TO DO
VAR1_simu = function(n, mu, skip = 300, Sigma.mat, rho, err.dist = "t3"){
  p = dim(Sigma.mat)[1]
  X = matrix(NA, nrow = p, ncol = n+skip+1)
  if(err.dist == "normal"){
    X[,1] = (1-rho)*mu + Sigma.mat %*% rnorm(p)
    for(i in 1:(n+skip)){
      X[,i+1] = (1-rho)*mu + rho * X[,i] + Sigma.mat %*% rnorm(p)
    }
  }else if(err.dist == "pareto"){
    X[,1] = (1-rho)*mu + Sigma.mat %*% (2*(rpareto(p, a = 3, b = 1) - 3/2)/sqrt(3))
    for(i in 1:(n+skip)){
      X[,i+1] = (1-rho)*mu + rho * X[,i] + Sigma.mat %*% (2*(rpareto(p, a = 3, b = 1) - 3/2)/sqrt(3))
    }
  }else if(err.dist == "lognorm"){
    X[,1] = (1-rho)*mu + Sigma.mat %*% (exp(rnorm(p)) - exp(1/2))/sqrt(exp(2)-exp(1))
    for(i in 1:(n+skip)){
      X[,i+1] = (1-rho)*mu + rho * X[,i] + Sigma.mat %*% (exp(rnorm(p)) - exp(1/2))/sqrt(exp(2)-exp(1))
    }
  }else if(startsWith(err.dist, "t") && as.numeric(substring(err.dist, 2)) > 2){
    df.t = as.numeric(substring(err.dist, 2))
    X[,1] = (1-rho)*mu + Sigma.mat %*% rt(p,df = df.t)/sqrt(df.t/(df.t - 2))
    for(i in 1:(n+skip)){
      X[,i+1] = (1-rho)*mu + rho * X[,i] + Sigma.mat %*% rt(p,df = df.t)/sqrt(df.t/(df.t - 2))
    }
  }else{
    stop("err.dist should be correctly specified.")
  }
  return(X[,(skip+2):(n+skip+1)])
}



#' @title Block cross-validation to select the truncation parameter tau for univariate time series
#' @description TO COMPLETE
#' @param x       A \code{numeric} vector containing observations of a univariate time series.
#' @param S       A strictly positive integer corresponding to the chosen number of non-overlapping blocks.
#' @param h       A strictly positive integer corresponding to how many neighboring blocks will be removed.
#' @param tau.vec A \code{numeric} vector of set of candidates of tuning parameter.
#' @param M.est   A \code{logical} scalar indicating which truncated estimator will be used (F: truncated mean estimator; T: adaptive huber M-estimator).
#' @return A \code{list} with the structure:
#' \itemize{
#'  \item tau.vec    A \code{numeric} vector of set of candidates of tuning parameter.
#'  \item err.cv     A \code{numeric} vector of cross validation error (in l2 norm), corresponding to tau.vec.
#' }
#' @export
#' @author Haotian Xu
#' @examples
#' TO DO
block.cv = function(x, S, h = 1, tau.vec, M.est = F){
  err.cv = rep(NA, length(tau.vec))
  n = length(x)
  m = floor(n/S)
  idx.ending = sample(m:n, size = S)
  if(M.est == F){
    for(j in 1:length(tau.vec)){
      err = rep(NA, 2*S)
      for(i in 1:S){
        x.test = x[c(((i-1)*m+1):(i*m))]
        x.train = x[-c(max(1, (i-h-1)*m+1):min((i+h)*m, n))]
        diff.test = x.test - trunc_mean(x.train, tau.vec[j])
        err[i] = sum(abs(diff.test))
      }
      for(l in 1:S){
        x.test = x[(idx.ending[l]-m+1):idx.ending[l]]
        x.train = x[-c(max(1, idx.ending[l]-(h+1)*m+1):min(idx.ending[l]+h*m, n))]
        diff.test = x.test - trunc_mean(x.train, tau.vec[j])
        err[l+S] = sum(abs(diff.test))
      }
      err.cv[j] = mean(err)
    }
  }else{
    for(j in 1:length(tau.vec)){
      err = rep(NA, 2*S)
      for(i in 1:S){
        x.test = x[c(((i-1)*m+1):(i*m))]
        x.train = x[-c(max(1, (i-h-1)*m+1):min((i+h)*m, n))]
        diff.test = x.test - huber_mean(x.train, tau.vec[j])
        err[i] = sum((diff.test)^2)
      }
      for(l in 1:S){
        x.test = x[(idx.ending[l]-m+1):idx.ending[l]]
        x.train = x[-c(max(1, idx.ending[l]-(h+1)*m+1):min(idx.ending[l]+h*m, n))]
        diff.test = x.test - huber_mean(x.train, tau.vec[j])
        err[l+S] = sum((diff.test)^2)
      }
      err.cv[j] = mean(err)
    }
  }
  return(err.cv)
}



# 5. Optimal tau
# X: p-by-n matrix; S: number of non-overlapping blocks; h: removing neighboring h blocks; max.tau: upper bound of the candidates of tuning parameter

#' @title Selecting the optimal tau for each coordinate of a multivariate time series
#' @description TO COMPLETE
#' @param X       A \code{numeric} pxn matrix containing n observations of a p-dimensional time series.
#' @param S       A strictly positive integer corresponding to the chosen number of non-overlapping blocks.
#' @param h       A strictly positive integer corresponding to how many neighboring blocks will be removed.
#' @param tau.vec A \code{numeric} vector of set of candidates of tuning parameter.
#' @return tau.cv A \code{numeric} vector of selected tuning parameters for all coordinates.
#' @param M.est   A \code{logical} scalar indicating which truncated estimator will be used (F: truncated mean estimator; T: adaptive huber M-estimator).
#' @export
#' @author Haotian Xu
#' @examples
#' TO DO
optim.tuning = function(X, S, h = 1, max.tau = 10, length_tau = 100, M_est = F){
  p = dim(X)[1]
  cv = sd = vector(mode = "list", length = p)
  start.trunc = apply(X, MARGIN = 1, FUN = function(x){quantile(abs(x), probs = 0.97)})
  tau_list = vector(mode = "list", length = p)
  if(M_est == F){
    for(j in 1:p){
      tau_list[[j]] = seq(from = start.trunc[j], to = max(abs(X[j,])), length.out = length_tau)
      cv[[j]] = block_cv(X[j,], S, h = h, tau_vec = tau_list[[j]], M_est = M_est)
    }
    tau.cv = tau.cv.sd = rep(NA, p)
    for(j in 1:p){
      tau.cv[j] = tau_list[[j]][which.min(cv[[j]])]
    }
  }else{
    for(j in 1:p){
      tau_list[[j]] = seq(from = 0.005, to = max.tau, length.out = length_tau)
      cv[[j]] = block_cv(X[j,], S, h = h, tau_vec = tau_list[[j]], M_est = M_est)
    }
    tau.cv = tau.cv.sd = rep(NA, p)
    for(j in 1:p){
      tau.cv[j] = tau_list[[j]][which.min(cv[[j]])]
    }
  }
  return(tau.cv)
}


# 5. Optimal tau
# X: p-by-n matrix; S: number of non-overlapping blocks; h: removing neighboring h blocks; max.tau: upper bound of the candidates of tuning parameter

#' @title THIS IS A TEST Selecting the optimal tau for each coordinate of a multivariate time series
#' @description TO COMPLETE
#' @param X       A \code{numeric} pxn matrix containing n observations of a p-dimensional time series.
#' @param S       A strictly positive integer corresponding to the chosen number of non-overlapping blocks.
#' @param h       A strictly positive integer corresponding to how many neighboring blocks will be removed.
#' @param tau.vec A \code{numeric} vector of set of candidates of tuning parameter.
#' @return tau.cv A \code{numeric} vector of selected tuning parameters for all coordinates.
#' @param M.est   A \code{logical} scalar indicating which truncated estimator will be used (F: truncated mean estimator; T: adaptive huber M-estimator).
#' @export
#' @author Haotian Xu
#' @examples
#' TO DO
optim.tuning2 = function(X, S, h = 1, length_tau = 5, M_est = F, max.tau = 10, start.prob = 0.97){
  p = dim(X)[1]
  start.trunc = apply(X, MARGIN = 1, FUN = function(x){quantile(abs(x), probs = start.prob)})
  start.trunc2 = apply(X, MARGIN = 1, FUN = function(x){quantile(abs(x - huber_mean(x, quantile(abs(x), probs = start.prob))), probs = start.prob)})
  tau_list = vector(mode = "list", length = p)
  if(M_est == F){
    tau.cv = rep(NA, p)
    for(j in 1:p){
      tau.cv[j] = get_cv_test(X[j,], S = S, h = h, start_val = start.trunc[j], end_val = max(abs(X[j,])), nb_iter = 2, length_tau = length_tau, M_est = M_est)
    }
  }else{
    tau.cv = rep(NA, p)
    for(j in 1:p){
      tau.cv[j] = get_cv_test(X[j,], S = S, h = h, start_val = start.trunc2[j], end_val = max.tau, nb_iter = 2, length_tau = length_tau, M_est = M_est)
    }
  }
  return(tau.cv)
}


get_cv_test = function(x, S = S, h = h, start_val, end_val, nb_iter = 4, length_tau, M_est = M_est){
  for (iter in 1:nb_iter){
    tau_vect = seq(from = start_val, to = end_val, length.out = length_tau)
    inter = block_cv(x, S, h = h, tau_vec = tau_vect, M_est = M_est)
    ind = which.min(inter)
    if (ind == 1){
      start_val = tau_vect[1]
      end_val = tau_vect[2]
    }else{
      if (ind == length_tau){
        start_val =  tau_vect[length_tau - 1]
        end_val = tau_vect[length_tau]
      }else{
        d_up = inter[ind + 1] - min(inter)
        d_dw = inter[ind - 1] - min(inter)
        delta = d_up/(d_up + d_dw)
        start_val =  tau_vect[ind] + (1 - delta)*(tau_vect[ind - 1] - tau_vect[ind])
        end_val = tau_vect[ind] + (delta)*(tau_vect[ind + 1] - tau_vect[ind])
      }
    }
  }
  tau_vect[which.min(inter)]
}





###########################################################
#' @title Block cross-validation to select the spectrum-wise truncation parameter tau for autocovariance
#' @description TO COMPLETE
#' @param X       A pxn matrix of observated multivariate time series.
#' @param S       A strictly positive integer corresponding to the chosen number of non-overlapping blocks.
#' @param h       A strictly positive integer corresponding to how many neighboring blocks will be removed.
#' @param tau.vec A \code{numeric} vector of set of candidates of tuning parameter (positive values).
#' @return A \code{list} with the structure:
#' \itemize{
#'  \item tau.vec    A \code{numeric} vector of set of candidates of tuning parameter.
#'  \item err.cv     A \code{numeric} vector of cross validation error (in l2 norm), corresponding to tau.vec.
#' }
#' @export
#' @author Haotian Xu
#' @examples
#' TO DO
spec_block.cv = function(X, S, h = 1, lag, tau.vec){
  err.cv = rep(NA, length(tau.vec))
  n = dim(X)[2]
  m = floor(n/S)
  idx.ending = sample(m:n, size = S)
  for(j in 1:length(tau.vec)){
    err = rep(NA, 2*S)
    for(i in 1:S){
      X.test = X[,c(((i-1)*m+1):(i*m))]
      X.train = X[,-c(max(1, (i-h-1)*m+1):min((i+h)*m, n))]
      err[i] = norm(X.test[,1:(m-lag)] %*% t(X.test[,(1+lag):m])/(m-lag) - spec_trunc_autocov(X.train, l=lag, tau.vec[j]), type = "2")
    }
    for(l in 1:S){
      X.test = X[,(idx.ending[l]-m+1):idx.ending[l]]
      X.train = X[,-c(max(1, idx.ending[l]-(h+1)*m+1):min(idx.ending[l]+h*m, n))]
      err[l+S] = norm(X.test[,1:(m-lag)] %*% t(X.test[,(1+lag):m])/(m-lag) - spec_trunc_autocov(X.train, l=lag, tau.vec[j]), type = "2")
    }
    err.cv[j] = mean(err)
  }
  outlist<-list(err.cv = err.cv, tau.vec = tau.vec)
  return(outlist)
}

