# VAR(1) Model: AutoCov
# n = 100 and n = 200
# p = c(50, 75, 100)

library(rcov)
n = 100 # sample size n = 200
p = 10#c(50, 75, 100) # p = seq(60, 300, 60) dimensionality
B = 10 # B = 200 replications
dist = c("normal", "pareto", "lognorm", "t4")
rho = 0.4 # A scalar in (0,1) indicating strength of the temporal dependence.
S = 10

# Population autocovariance matrices
Gamma_pop = function(Sigma, l, rho){
  if(isSymmetric(Sigma) == FALSE){
    stop("Sigma should be a symmetric matrix.")
  }
  if(rho < 0 | rho > 1){
    stop("rho should be in (0,1).")
  }
  Gamma_l = (1 - rho^2)^(-1)*rho^(abs(l))*Sigma
  return(Gamma_l)
}

# Covariance of the error term
Sigma_err = function(p, type){
  if(type == 1){
    Sigma = diag(1, p)
  }else if(type == 2){
    Sigma = matrix(0.5, nrow = p, ncol = p) + diag(0.5, p)
  }else if(type == 3){
    Sigma = matrix(NA, nrow = p, ncol = p)
    for(i in 1:p){
      for(j in 1:p){
        Sigma[i,j] = 0.5^(abs(i-j))
      }
    }
  }
  return(Sigma)
}


results = array(NA, dim = c(B, 3, 7, length(p), length(dist), 3, 3))

pb = txtProgressBar(min = 0, max = B*length(p)*length(dist)*3, style = 3)
counter = 0

set.seed(123)
for (k in 1:length(p)){
  temp.mat = matrix(NA, nrow = p[k], ncol = p[k])
  for(kk in 1:3){
    Sigma = Sigma_err(p[k], type = kk)
    Sigma.mat = expm::sqrtm(Sigma)
    Gamma0 = Gamma_pop(Sigma, 0, rho)
    Gamma1 = Gamma_pop(Sigma, 1, rho)
    Gamma2 = Gamma_pop(Sigma, 2, rho)

    for (h in 1:length(dist)){
      for(b in 1:B){
        X = VAR1_simu(n = n, mu = rep(0, p[k]), skip = 300, Sigma.mat = Sigma.mat, rho = rho, err.dist = dist[h])

        spec_norm_crossprod0.vec = rep(NA, n)
        spec_norm_crossprod1.vec = rep(NA, n-1)
        spec_norm_crossprod2.vec = rep(NA, n-2)
        crossprod0.mat = matrix(NA, nrow = (p[k])^2, ncol = n)
        crossprod1.mat = matrix(NA, nrow = (p[k])^2, ncol = n-1)
        crossprod2.mat = matrix(NA, nrow = (p[k])^2, ncol = n-2)
        for(i in 1:n){
          crossprod0 = X[,i] %*% t(X[,i])
          spec_norm_crossprod0.vec[i] = norm(crossprod0, type = "2")
          crossprod0.mat[,i] = c(crossprod0)
        }
        for(i in 1:(n-1)){
          crossprod1 = X[,i] %*% t(X[,i+1])
          spec_norm_crossprod1.vec[i] = norm(crossprod1, type = "2")
          crossprod1.mat[,i] = c(crossprod1)
        }
        for(i in 1:(n-2)){
          crossprod2 = X[,i] %*% t(X[,i+2])
          spec_norm_crossprod2.vec[i] = norm(crossprod2, type = "2")
          crossprod2.mat[,i] = c(crossprod2)
        }

        temp.mat = matrix(apply(crossprod0.mat, MARGIN = 1, mean), nrow = p[k]) - Gamma0
        results[b,1,1,k,h,kk,1] = max(abs(temp.mat))
        results[b,1,1,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,1,1,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(crossprod0.mat, MARGIN = 1, median), nrow = p[k]) - Gamma0
        results[b,1,2,k,h,kk,1] = max(abs(temp.mat))
        results[b,1,2,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,1,2,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(crossprod0.mat, MARGIN = 1, FUN = function(x){MoM_mean(x, 10)}), nrow = p[k]) - Gamma0
        results[b,1,3,k,h,kk,1] = max(abs(temp.mat))
        results[b,1,3,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,1,3,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(crossprod0.mat, MARGIN = 1, FUN = function(x){MoM_mean(x, 20)}), nrow = p[k]) - Gamma0
        results[b,1,4,k,h,kk,1] = max(abs(temp.mat))
        results[b,1,4,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,1,4,k,h,kk,3] = norm(temp.mat, type = "F")


        tau1_cv.vec = optim.tuning(crossprod0.mat, S = 10, h = 1, M_est = F)
        tau2_cv.vec = optim.tuning(crossprod0.mat, S = 10, h = 1, M_est = T)
        temp.mat = matrix(apply(cbind(crossprod0.mat, tau1_cv.vec), MARGIN = 1, FUN = function(x){trunc_mean(x[-length(x)], x[length(x)])}), nrow = p[k]) - Gamma0
        results[b,1,5,k,h,kk,1] = max(abs(temp.mat))
        results[b,1,5,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,1,5,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(cbind(crossprod0.mat, tau2_cv.vec), MARGIN = 1, FUN = function(x){huber_mean(x[-length(x)], x[length(x)])}), nrow = p[k]) - Gamma0
        results[b,1,6,k,h,kk,1] = max(abs(temp.mat))
        results[b,1,6,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,1,6,k,h,kk,3] = norm(temp.mat, type = "F")

        tau.vec = seq(from = quantile(spec_norm_crossprod0.vec, probs = 0.95), to = max(spec_norm_crossprod0.vec), length.out = 50)
        temp.mat = spec_trunc_autocov(X, 0, tau.vec[which.min(spec_block_cv(X, S = 10, h = 1, lag = 0, tau.vec))]) - Gamma0
        results[b,1,7,k,h,kk,1] = max(abs(temp.mat))
        results[b,1,7,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,1,7,k,h,kk,3] = norm(temp.mat, type = "F")


        temp.mat = matrix(apply(crossprod1.mat, MARGIN = 1, mean), nrow = p[k]) - Gamma1
        results[b,2,1,k,h,kk,1] = max(abs(temp.mat))
        results[b,2,1,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,2,1,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(crossprod1.mat, MARGIN = 1, median), nrow = p[k]) - Gamma1
        results[b,2,2,k,h,kk,1] = max(abs(temp.mat))
        results[b,2,2,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,2,2,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(crossprod1.mat, MARGIN = 1, FUN = function(x){MoM_mean(x, 10)}), nrow = p[k]) - Gamma1
        results[b,2,3,k,h,kk,1] = max(abs(temp.mat))
        results[b,2,3,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,2,3,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(crossprod1.mat, MARGIN = 1, FUN = function(x){MoM_mean(x, 20)}), nrow = p[k]) - Gamma1
        results[b,2,4,k,h,kk,1] = max(abs(temp.mat))
        results[b,2,4,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,2,4,k,h,kk,3] = norm(temp.mat, type = "F")


        tau1_cv.vec = optim.tuning(crossprod1.mat, S = 10, h = 1, M_est = F)
        tau2_cv.vec = optim.tuning(crossprod1.mat, S = 10, h = 1, M_est = T)
        temp.mat = matrix(apply(cbind(crossprod1.mat, tau1_cv.vec), MARGIN = 1, FUN = function(x){trunc_mean(x[-length(x)], x[length(x)])}), nrow = p[k]) - Gamma1
        results[b,2,5,k,h,kk,1] = max(abs(temp.mat))
        results[b,2,5,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,2,5,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(cbind(crossprod1.mat, tau2_cv.vec), MARGIN = 1, FUN = function(x){huber_mean(x[-length(x)], x[length(x)])}), nrow = p[k]) - Gamma1
        results[b,2,6,k,h,kk,1] = max(abs(temp.mat))
        results[b,2,6,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,2,6,k,h,kk,3] = norm(temp.mat, type = "F")

        tau.vec = seq(from = quantile(spec_norm_crossprod1.vec, probs = 0.95), to = max(spec_norm_crossprod1.vec), length.out = 50)
        temp.mat = spec_trunc_autocov(X, 1, tau.vec[which.min(spec_block_cv(X, S = 10, h = 1, lag = 1, tau.vec))]) - Gamma1
        results[b,2,7,k,h,kk,1] = max(abs(temp.mat))
        results[b,2,7,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,2,7,k,h,kk,3] = norm(temp.mat, type = "F")


        temp.mat = matrix(apply(crossprod2.mat, MARGIN = 1, mean), nrow = p[k]) - Gamma2
        results[b,3,1,k,h,kk,1] = max(abs(temp.mat))
        results[b,3,1,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,3,1,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(crossprod2.mat, MARGIN = 1, median), nrow = p[k]) - Gamma2
        results[b,3,2,k,h,kk,1] = max(abs(temp.mat))
        results[b,3,2,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,3,2,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(crossprod2.mat, MARGIN = 1, FUN = function(x){MoM_mean(x, 10)}), nrow = p[k]) - Gamma2
        results[b,3,3,k,h,kk,1] = max(abs(temp.mat))
        results[b,3,3,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,3,3,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(crossprod2.mat, MARGIN = 1, FUN = function(x){MoM_mean(x, 20)}), nrow = p[k]) - Gamma2
        results[b,3,4,k,h,kk,1] = max(abs(temp.mat))
        results[b,3,4,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,3,4,k,h,kk,3] = norm(temp.mat, type = "F")

        tau1_cv.vec = optim.tuning(crossprod2.mat, S = 10, h = 1, M_est = F)
        tau2_cv.vec = optim.tuning(crossprod2.mat, S = 10, h = 1, M_est = T)

        temp.mat = matrix(apply(cbind(crossprod2.mat, tau1_cv.vec), MARGIN = 1, FUN = function(x){trunc_mean(x[-length(x)], x[length(x)])}), nrow = p[k]) - Gamma2
        results[b,3,5,k,h,kk,1] = max(abs(temp.mat))
        results[b,3,5,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,3,5,k,h,kk,3] = norm(temp.mat, type = "F")

        temp.mat = matrix(apply(cbind(crossprod2.mat, tau2_cv.vec), MARGIN = 1, FUN = function(x){huber_mean(x[-length(x)], x[length(x)])}), nrow = p[k]) - Gamma2
        results[b,3,6,k,h,kk,1] = max(abs(temp.mat))
        results[b,3,6,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,3,6,k,h,kk,3] = norm(temp.mat, type = "F")

        tau.vec = seq(from = quantile(spec_norm_crossprod2.vec, probs = 0.95), to = max(spec_norm_crossprod2.vec), length.out = 50)
        temp.mat = spec_trunc_autocov(X, 2, tau.vec[which.min(spec_block_cv(X, S = 10, h = 1, lag = 2, tau.vec))]) - Gamma2
        results[b,3,7,k,h,kk,1] = max(abs(temp.mat))
        results[b,3,7,k,h,kk,2] = norm(temp.mat, type = "2")
        results[b,3,7,k,h,kk,3] = norm(temp.mat, type = "F")

        counter = counter + 1
        setTxtProgressBar(pb, counter)
      }
    }
  }
}
close(pb)

#save(n, p, results, file = "simu_cov_var1.RData")


