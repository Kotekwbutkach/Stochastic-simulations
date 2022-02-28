m = 1000

n = 9
U = runif(max = m)
X = rbinom(m, size = n, prob = U)
hist(X)
