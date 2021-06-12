

# Pareto
qqplot(sqrt(100)*results[,5,1,2,1], GA.results[,1,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,1,2,2], GA.results[,1,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,1,2,3], GA.results[,1,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,5,2,2,1], GA.results[,2,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,2,2,2], GA.results[,2,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,2,2,3], GA.results[,2,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,5,3,2,1], GA.results[,3,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,3,2,2], GA.results[,3,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,3,2,3], GA.results[,3,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Power decay")
abline(a = 0, b = 1, col = "grey")


# Log-norm
qqplot(sqrt(100)*results[,5,1,3,1], GA.results[,1,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,1,3,2], GA.results[,1,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,1,3,3], GA.results[,1,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,5,2,3,1], GA.results[,2,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,2,3,2], GA.results[,2,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,2,3,3], GA.results[,2,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,5,3,3,1], GA.results[,3,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,3,3,2], GA.results[,3,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,3,3,3], GA.results[,3,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Power decay")
abline(a = 0, b = 1, col = "grey")


# t3
qqplot(sqrt(100)*results[,5,1,4,1], GA.results[,1,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,1,4,2], GA.results[,1,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,1,4,3], GA.results[,1,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,5,2,4,1], GA.results[,2,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,2,4,2], GA.results[,2,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,2,4,3], GA.results[,2,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,5,3,4,1], GA.results[,3,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,3,4,2], GA.results[,3,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,5,3,4,3], GA.results[,3,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Power decay")
abline(a = 0, b = 1, col = "grey")







# AH
# Normal
qqplot(sqrt(100)*results[,6,1,1,1], GA.results[,1,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,1,1,2], GA.results[,1,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,1,1,3], GA.results[,1,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,6,2,1,1], GA.results[,2,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,2,1,2], GA.results[,2,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,2,1,3], GA.results[,2,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,6,3,1,1], GA.results[,3,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,3,1,2], GA.results[,3,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,3,1,3], GA.results[,3,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Power decay")
abline(a = 0, b = 1, col = "grey")


# Pareto
qqplot(sqrt(100)*results[,6,1,2,1], GA.results[,1,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,1,2,2], GA.results[,1,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,1,2,3], GA.results[,1,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,6,2,2,1], GA.results[,2,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,2,2,2], GA.results[,2,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,2,2,3], GA.results[,2,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,6,3,2,1], GA.results[,3,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,3,2,2], GA.results[,3,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,3,2,3], GA.results[,3,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Power decay")
abline(a = 0, b = 1, col = "grey")


# Log-norm
qqplot(sqrt(100)*results[,6,1,3,1], GA.results[,1,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,1,3,2], GA.results[,1,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,1,3,3], GA.results[,1,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,6,2,3,1], GA.results[,2,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,2,3,2], GA.results[,2,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,2,3,3], GA.results[,2,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,6,3,3,1], GA.results[,3,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,3,3,2], GA.results[,3,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,3,3,3], GA.results[,3,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Power decay")
abline(a = 0, b = 1, col = "grey")


# t3
qqplot(sqrt(100)*results[,6,1,4,1], GA.results[,1,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,1,4,2], GA.results[,1,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,1,4,3], GA.results[,1,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 50, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,6,2,4,1], GA.results[,2,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,2,4,2], GA.results[,2,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,2,4,3], GA.results[,2,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 75, Power decay")
abline(a = 0, b = 1, col = "grey")

qqplot(sqrt(100)*results[,6,3,4,1], GA.results[,3,1], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Diagonal")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,3,4,2], GA.results[,3,2], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Equal correlation")
abline(a = 0, b = 1, col = "grey")
qqplot(sqrt(100)*results[,6,3,4,3], GA.results[,3,3], xlab = "GA Quantiles", ylab = "Ele.T Error Quantiles", main = "p = 100, Power decay")
abline(a = 0, b = 1, col = "grey")
