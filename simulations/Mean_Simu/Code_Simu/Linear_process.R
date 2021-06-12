# Linear process Model
# n = 100 and n = 200
# p = c(50, 75, 100)
rm(list = ls())
library(rcov)
n = 100 # sample size n = 200
p = c(50, 100, 150) # p = c(50, 75, 100) dimensionality
B = 500 # B = 200 replications
dist = c("normal", "pareto", "lognorm", "t3")
K = 1001 #(K-1) represents the number of lags considered in the linear process model.
rho = 0.4 # A scalar in (0,1) indicating the decay rate of the transition matrix for lag-k term as the k grows.


# Population autocovariance matrices
Gamma_pop = function(M.list, l, rho){
  K = length(M.list)
  d = dim(M.list[[1]])[1]
  Gamma_l = matrix(0, nrow = d, ncol = d)
  if(l == 0){
    for(i in 1:K){
      Gamma_l = Gamma_l + rho^(2*(i-1))*M.list[[i]] %*% t(M.list[[i]])
    }
  }else if(l > 0){
    for(i in 1:(K-l)){
      Gamma_l = Gamma_l + rho^(2*(i-1)+l)*M.list[[i]] %*% t(M.list[[i+l]])
    }
  }
  return(Gamma_l)
}


results = array(NA, dim = c(B, 6, length(p), length(dist)))
GA.results = array(NA, dim = c(B, length(p)))
pb = txtProgressBar(min = 0, max = B*length(p)*length(dist), style = 3)
counter = 0

for (k in 1:length(p)){
  set.seed(111)
  #mu = runif(p[k], min = 1, max = 10) * sample(c(-1,1), p[k], replace=T)
  mu = rep(0, p[k])
  M.list = vector(mode = "list", length = K)
  # A list of p x p matrices, and (rho^(k-1)*M.list[[k]]) be the transition matrix for lag-k term.
  for(ll in 1:K){
    M.list[[ll]] = matrix(rnorm(p[k]^2), nrow = p[k])
  }

  LRV.mat = matrix(0, nrow = p[k], ncol = p[k])
  for(lag in 1:500){
    LRV.mat = LRV.mat + Gamma_pop(M.list, lag, rho)
  }
  LRV.mat = LRV.mat + t(LRV.mat) + Gamma_pop(M.list, 0, rho)
  for(b in 1:B){
    GA.results[b,k] = max(abs(expm::sqrtm(LRV.mat) %*% rnorm(p[k])))
  }

  for (h in 1:length(dist)){
    for(b in 1:B){
      X = linear_simu(n = n, mu = mu, M.list = M.list, rho = rho, K = 1001, err.dist = dist[h])
      results[b,1,k,h] = max(abs(apply(X, MARGIN = 1, mean) - mu))
      results[b,2,k,h] = max(abs(apply(X, MARGIN = 1, median) - mu))
      results[b,3,k,h] = max(abs(apply(X, MARGIN = 1, FUN = function(y){MoM_mean2(y, 10)$mom}) - mu))
      results[b,4,k,h] = max(abs(apply(X, MARGIN = 1, FUN = function(y){MoM_mean2(y, 20)$mom}) - mu))

      tau.cv = optim.tuning2(X, S = 10, h = 1, M_est = F)
      tau2.cv = optim.tuning2(X, S = 10, h = 1, M_est = T)
      results[b,5,k,h] = max(abs(apply(cbind(X, tau.cv), MARGIN = 1, FUN = function(y){trunc_mean(y[1:n], y[n+1])}) - mu))
      results[b,6,k,h] = max(abs(apply(cbind(X, tau2.cv), MARGIN = 1, FUN = function(y){huber_mean(y[1:n], y[n+1])}) - mu))
      counter = counter + 1
      setTxtProgressBar(pb, counter)
    }
  }
}
close(pb)


save(results, GA.results, file = "simu_mean_lp.RData")


