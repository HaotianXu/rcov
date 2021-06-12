library(matlib)
library(SparseM)

#1. VAR2 Model
n = 200
p = 60
B = 500
set.seed(1111)

k = 1

A1.tilde = toeplitz(seq(0.5, 0, length.out = 5))
A2.tilde = diag(c(-0.2, 0.3, 0.2, -0.1, 0.1))
A1 = kronecker(diag(rep(1,p[k]/5)), A1.tilde)
A2 = kronecker(diag(rep(1,p[k]/5)), A2.tilde)
A1.csr = as.matrix.csr(A1)
A2.csr = as.matrix.csr(A2)

Gamma.tilde = matrix(NA, nrow = p, ncol = p)
for(i in 1:p){
  for(j in 1:p){
    Gamma.tilde[i,j] = 0.25^(abs(i-j))
  }
}
Gamma.mat = expm::sqrtm(Gamma.tilde)

## computing auto-covariance matrices
Sigma_b = cbind(rbind(Gamma.tilde, matrix(0, nrow = p[k], ncol = p[k])), matrix(0, nrow = 2*p[k], ncol = p[k]))
Phi_1 = cbind(rbind(A1.csr, as.matrix.csr(diag(rep(1, p[k])))), rbind(A2.csr, as.matrix.csr(matrix(0, nrow = p[k], ncol = p[k]))))
Phi_2 = as.matrix.csr(diag(rep(1, (2*p[k])^2))) - kronecker(Phi_1, Phi_1)
g.vec = solve(Phi_2, c(Sigma_b))
g.mat = matrix(g.vec, nrow = 2*p[k])
Gamma_0 = g.mat[1:p[k], 1:p[k]]
Gamma_1 = g.mat[1:p[k], (p[k]+1):(2*p[k])]
Gamma_2 = A1 %*% Gamma_1 + A2 %*% Gamma_0
Gamma.list = vector(mode = "list", length = 1000)
Gamma.list[[1]] = Gamma_1
Gamma.list[[2]] = Gamma_2
for(j in 3:1000){
  Gamma.list[[j]] = A1 %*% Gamma.list[[j-1]] + A2 %*% Gamma.list[[j-2]]
}
LRV_var2 = Reduce('+', Gamma.list) + t(Reduce('+', Gamma.list)) + Gamma_0
# for(b in 1:B){
#   GA.results[b,k] = max(abs(expm::sqrtm(LRV_var2) %*% rnorm(p[k])))
# }



# # 1. VAR1 Model
n = 200
p = 100
B = 500
set.seed(1111)

k = 1

A.tilde = toeplitz(c(0.7, 0.5, 0.3, 0.1, rep(0, 6)))
A.csr = as.matrix.csr(kronecker(diag(rep(1,p[k]/10)), A.tilde))

Gamma.tilde = matrix(NA, nrow = p[k], ncol = p[k])
for(i in 1:p[k]){
  for(j in 1:p[k]){
    Gamma.tilde[i,j] = 0.25^(abs(i-j))
  }
}
Gamma.mat = expm::sqrtm(Gamma.tilde)

## computing auto-covariance matrices
Phi.csr = as.matrix.csr(diag(rep(1,(p[k])^2))) - kronecker(A.csr, A.csr)
g.vec = solve(Phi.csr, c(Gamma.tilde))
Gamma_0 = matrix(g.vec, nrow = p[k])
Gamma_1 = kronecker(diag(rep(1,p[k]/5)), A.tilde) %*% Gamma_0
Gamma_2 = kronecker(diag(rep(1,p[k]/5)), A.tilde) %*% Gamma_1
Gamma_3 = kronecker(diag(rep(1,p[k]/5)), A.tilde) %*% Gamma_2
Gamma_4 = kronecker(diag(rep(1,p[k]/5)), A.tilde) %*% Gamma_3
Gamma_5 = kronecker(diag(rep(1,p[k]/5)), A.tilde) %*% Gamma_4
Gamma_6 = kronecker(diag(rep(1,p[k]/5)), A.tilde) %*% Gamma_5
Gamma_7 = kronecker(diag(rep(1,p[k]/5)), A.tilde) %*% Gamma_6
Gamma_8 = kronecker(diag(rep(1,p[k]/5)), A.tilde) %*% Gamma_7
Gamma.list = vector(mode = "list", length = 1000)
Gamma.list[[1]] = Gamma_1
Gamma.list[[2]] = Gamma_2
for(j in 3:1000){
  Gamma.list[[j]] = A1 %*% Gamma.list[[j-1]] + A2 %*% Gamma.list[[j-2]]
}
LRV_var2 = Reduce('+', Gamma.list) + t(Reduce('+', Gamma.list)) + Gamma_0
# for(b in 1:B){
#   GA.results[b,k] = max(abs(expm::sqrtm(LRV_var2) %*% rnorm(p[k])))
# }




## Cross-validation
X = var2_simu(n, mu, skip = 300, A1 = A1, A2 = A2, Gamma.mat = Gamma.mat, err.dist = t5)
n = dim(X_var2_mu0.t5)[2]
S = 20 #ceiling(log(n)) # number of non-overlapping blocks
m = floor(n/S) # length of each block
h = 1 # removing neighboring h blocks
tau = seq(0.5, 50, 0.5)

cov_block.cv = function(X, Y, lag = 0, S, h, tau){
  ## X, Y: univariate time series; lag >= 0: lag; S: number of non-overlapping blocks; h: removing neighboring h blocks; tau: set of candidates of tuning parameter
  err.cv = rep(NA, length(tau))
  N = length(X)-lag
  m = floor(N/S)
  idx.ending = sample(m:N, size = S)
  if(lag == 0){
    X.vec = X
    Y.vec = Y
  }else if(lag > 0){
    X.vec = X[-(1:lag)]
    Y.vec = Y[1:N]
  }
  for(j in 1:length(tau)){
    err = rep(NA, 2*S)
    for(i in 1:S){
      X.test = X.vec[c(((i-1)*m+1):(i*m))]
      X.train = X.vec[-c(max(1, (i-h-1)*m+1):min((i+h)*m, N))]
      Y.test = Y.vec[c(((i-1)*m+1):(i*m))]
      Y.train = Y.vec[-c(max(1, (i-h-1)*m+1):min((i+h)*m, N))]
      diff.test = X.test*Y.test - Trunc_mean(X.train*Y.train, tau[j])
      err[i] = mean((diff.test)^2)
    }
    for(l in 1:S){
      X.test = X.vec[(idx.ending[l]-m+1):idx.ending[l]]
      X.train = X.vec[-c(max(1, idx.ending[l]-(h+1)*m+1):min(idx.ending[l]+h*m, N))]
      Y.test = Y.vec[(idx.ending[l]-m+1):idx.ending[l]]
      Y.train = Y.vec[-c(max(1, idx.ending[l]-(h+1)*m+1):min(idx.ending[l]+h*m, N))]
      diff.test = X.test*Y.test - Trunc_mean(X.train*Y.train, tau[j])
      err[l+S] = mean((diff.test)^2)
    }
    err.cv[j] = median(err)
  }
  outlist<-list(err.cv = err.cv, tau = tau)
  return(outlist)
}

cov_cv_lag0.t5 = vector(mode = "list", length = p^2)
for(i in 1:p){
  for(j in 1:p){
    cov_cv_lag0.t5[[(i-1)*p+j]] = cov_block.cv(X_var2_mu0.t5[i,], X_var2_mu0.t5[j,], lag = 0, S = 20, h = 1, tau = tau)$err.cv
  }
  print(i)
}

save(cov_cv_lag0.t5, tau, file = "high-dimensional_covariance_var2_t5_cv.RData")
tau_var2_cov_cv_lag0.t5 = (matrix(sapply(cov_cv_lag0.t5, FUN = function(y){tau[which.min(y)]}), nrow = p, byrow = TRUE) + t(matrix(sapply(cov_cv_lag0.t5, FUN = function(y){tau[which.min(y)]}), nrow = p, byrow = TRUE)))/2
diag(tau_var2_cov_cv_lag0.t5)


B = 100
### Compare covariance estimators
sample_mean_cov_var2.t5 = vector(mode = "list", length = B)
sample_median_cov_var2.t5 = vector(mode = "list", length = B)
mom1_cov_var2.t5 = vector(mode = "list", length = B)
mom2_cov_var2.t5 = vector(mode = "list", length = B)
huber_mean1_cov_var2.t5 = vector(mode = "list", length = B)
for(b in 1:B){
  X = var2_simu_mu0.t5(n, skip = 300, A1 = A1, A2 = A2, Gamma.mat = Gamma.mat)$series
  sample_mean_cov_var2.t5[[b]] = X %*% t(X)/n
  crossproduct.list = vector(mode = "list", length = n)
  for(i in 1:n){
    crossproduct.list[[i]] = X[,i] %*% t(X[,i])
  }
  elementwise_median.mat = matrix(NA, nrow = p, ncol = p)
  elementwise_mom1.mat = matrix(NA, nrow = p, ncol = p)
  elementwise_mom2.mat = matrix(NA, nrow = p, ncol = p)
  elementwise_huber.mat = matrix(NA, nrow = p, ncol = p)
  for(j in 1:p){
    for(k in 1:p){
      crossprod_element.vec = sapply(crossproduct.list, FUN = function(y) y[j,k])
      elementwise_median.mat[j,k] = median(crossprod_element.vec)
      elementwise_mom1.mat[j,k] = MoM_mean(crossprod_element.vec, 18)$mean_median
      elementwise_mom2.mat[j,k] = MoM_mean(crossprod_element.vec, 20)$mean_median
      elementwise_huber.mat[j,k] = Trunc_mean(crossprod_element.vec, tau_var2_cov_cv_lag0.t5[j,k])
    }
  }
  sample_median_cov_var2.t5[[b]] = elementwise_median.mat
  mom1_cov_var2.t5[[b]] = elementwise_mom1.mat
  mom2_cov_var2.t5[[b]] = elementwise_mom2.mat
  huber_mean1_cov_var2.t5[[b]] = elementwise_huber.mat
  print(b)
}


err_sample_mean_lag0.t5 = rep(NA, B)
err_sample_median_lag0.t5 = rep(NA, B)
err_mom1_lag0.t5 = rep(NA, B)
err_mom2_lag0.t5 = rep(NA, B)
err_huber_lag0.t5 = rep(NA, B)

for(b in 1:B){
  err_sample_mean_lag0.t5[b] = max(abs(sample_mean_cov_var2.t5[[b]] - Gamma_0))
  err_sample_median_lag0.t5[b] = max(abs(sample_median_cov_var2.t5[[b]] - Gamma_0))
  err_mom1_lag0.t5[b] = max(abs(mom1_cov_var2.t5[[b]] - Gamma_0))
  err_mom2_lag0.t5[b] = max(abs(mom2_cov_var2.t5[[b]] - Gamma_0))
  err_huber_lag0.t5[b] = max(abs(huber_mean1_cov_var2.t5[[b]] - Gamma_0))
}

boxplot(err_sample_mean_lag0.t5, err_sample_median_lag0.t5, err_mom1_lag0.t5, err_mom2_lag0.t5, err_huber_lag0.t5,
        names=c("sample_mean","sample_med","mom_18","mom_20","huber"),
        main="Var2 with t5",
        xlab="Covariance Estimators with lag0",
        ylab="Error in max-norm")
abline(h=(seq(10, 30, 2.5)), col="lightgray", lty=3)
