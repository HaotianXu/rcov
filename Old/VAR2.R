# VAR2 Model
## 1. With innovations following t3, and n = 200, p = seq(60, 300, 60)
## 2. With innovations following pareto, and n = 200, p = seq(60, 300, 60)
## 3. With innovations following lognormal, and n = 200, p = seq(60, 300, 60)

library(rcov)
n = 200 # sample size
p = c(60, 300) # p = seq(60, 300, 60) dimensionality
B = 500 # B = 100 replications
dist = c("normal", "pareto", "lognorm", "t3")

results = array(NA, dim = c(B, 8, length(p), length(dist)))
GA.results = array(NA, dim = c(B, length(p)))
pb = txtProgressBar(min = 0, max = B*length(p)*length(dist), style = 3)
counter = 0
for (k in 1:length(p)){
  set.seed(111)
  mu = runif(p[k], min = 1, max = 10) * sample(c(-1,1), p[k], replace=T)
  A1.tilde = matrix(c(0.7,0,0.3,0.1,0.4,0,0,0.1,0.5), nrow = 3)
  A2.tilde = matrix(c(-0.2,0,0,0,0.2,0,0,0.1,0.1), nrow = 3)
  A1 = kronecker(diag(rep(1,p[k]/3)), A1.tilde)
  A2 = kronecker(diag(rep(1,p[k]/3)), A2.tilde)

  Gamma.tilde = matrix(NA, nrow = p[k], ncol = p[k])
  for(i in 1:p[k]){
    for(j in 1:p[k]){
      Gamma.tilde[i,j] = 0.25^(abs(i-j))
    }
  }
  Gamma.mat = expm::sqrtm(Gamma.tilde)

  # Sigma_b = cbind(rbind(Gamma.tilde, matrix(0, nrow = p[k], ncol = p[k])), matrix(0, nrow = 2*p[k], ncol = p[k]))
  # Phi_1 = Matrix::Matrix(cbind(rbind(A1, diag(rep(1, p[k]))), rbind(A2, matrix(0, nrow = p[k], ncol = p[k]))), sparse = TRUE)
  # Phi_2 = Matrix::Matrix(Matrix::Diagonal((2*p[k])^2) - kronecker(Phi_1, Phi_1), sparse = TRUE)
  # g.vec = solve(Phi_2, c(Sigma_b), sparse = TRUE)
  # g.mat = matrix(g.vec, nrow = 2*p[k])
  # Gamma_0 = g.mat[1:p[k], 1:p[k]]
  # Gamma_1 = g.mat[1:p[k], (p[k]+1):(2*p[k])]
  # Gamma_2 = A1 %*% Gamma_1 + A2 %*% Gamma_0
  # Gamma.list = vector(mode = "list", length = 1000)
  # Gamma.list[[1]] = Gamma_1
  # Gamma.list[[2]] = Gamma_2
  # for(j in 3:1000){
  #   Gamma.list[[j]] = A1 %*% Gamma.list[[j-1]] + A2 %*% Gamma.list[[j-2]]
  # }
  # LRV_var2 = Reduce('+', Gamma.list) + t(Reduce('+', Gamma.list)) + Gamma_0
  # for(b in 1:B){
  #   GA.results[b,k] = max(abs(expm::sqrtm(LRV_var2) %*% rnorm(p[k])))
  # }

  for (h in 1:length(dist)){
    for(b in 1:B){
      X = var2_simu(n, mu, skip = 300, A1 = A1, A2 = A2, Gamma.mat = Gamma.mat, err.dist = dist[h])

      results[b,1,k,h] = max(abs(apply(X, MARGIN = 1, mean) - mu))
      results[b,2,k,h] = max(abs(apply(X, MARGIN = 1, median) - mu))
      results[b,3,k,h] = max(abs(apply(X, MARGIN = 1, FUN = function(y){MoM_mean2(y, 40)$mom}) - mu))
      #results[b,4,k,h] = max(abs(apply(X, MARGIN = 1, FUN = function(y){MoM_mean2(y, 42)$mom}) - mu))
      results[b,4,k,h] = max(abs(apply(X, MARGIN = 1, FUN = function(y){MoM_mean2(y, 50)$mom}) - mu))
      #results[b,6,k,h] = max(abs(apply(X, MARGIN = 1, FUN = function(y){MoM_mean2(y, 52)$mom}) - mu))

      tau2.cv = optim.tuning(X, S = 10, h = 1, M.est = T)
      tau.cv = optim.tuning(X, S = 10, h = 1, M.est = F)

      results[b,5,k,h] = max(abs(apply(cbind(X, tau.cv), MARGIN = 1, FUN = function(y){trunc_mean(y[1:n], y[n+1])}) - mu))
      results[b,6,k,h] = max(abs(apply(cbind(X, tau2.cv), MARGIN = 1, FUN = function(y){huber_mean(y[1:n], y[n+1])}) - mu))

      #results[b,7,k,h] = max(abs(apply(cbind(X, tau.cv$sd), MARGIN = 1, FUN = function(y){trunc_mean(y[1:n], y[n+1])}) - mu))
      #results[b,8,k,h] = max(abs(apply(cbind(X, tau2.cv$sd), MARGIN = 1, FUN = function(y){huber_mean(y[1:n], y[n+1])}) - mu))
      counter = counter + 1
      setTxtProgressBar(pb, counter)
    }
  }
}
close(pb)


save(n, p, results, GA.results, file = "simu_mean_VAR2.RData")

