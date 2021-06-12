# VAR1 Model
# n = 100 and n = 200
# p = c(50, 75, 100)
rm(list = ls())
library(rcov)
n = 100 # sample size n = 200
p = c(50, 100, 150) # p = c(50, 75, 100) dimensionality
B = 500 # B = 200 replications
dist = c("normal", "pareto", "lognorm", "t3")
rho = 0.4 # A scalar in (0,1) indicating the decay rate of the transition matrix for lag-k term as the k grows.


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


GA.results = array(NA, dim = c(B, length(p), 3))
pb = txtProgressBar(min = 0, max = B*length(p)*length(dist)*3, style = 3)
counter = 0

set.seed(123)
for (k in 1:length(p)){
  for(kk in 1:3){
    LRV.mat = matrix(0, nrow = p[k], ncol = p[k])
    Sigma = Sigma_err(p[k], type = kk)
    Sigma.mat = expm::sqrtm(Sigma)
    
    for(lag in 1:500){
      LRV.mat = LRV.mat + Gamma_pop(Sigma, lag, rho)
    }
    LRV.mat = LRV.mat + t(LRV.mat) + Gamma_pop(Sigma, 0, rho)
    for(b in 1:B){
      GA.results[b,k,kk] = max(abs(expm::sqrtm(LRV.mat) %*% rnorm(p[k])))
      setTxtProgressBar(pb, counter)
    }
  }
}
close(pb)






save(GA.results, file = "GA_mean_var1.RData")


