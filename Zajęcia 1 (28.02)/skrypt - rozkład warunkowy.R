m <- 10000

k <- 9

X <- NULL

U_x <- rep(list(rep(NA, m)), k+1)

for (i in 1:m)
{
  U <- runif(1)
  x <- rbinom(1, size = k, prob = U)
  X <- append(X, x)
  U_x[[x+1]][i] <- U
}

par(mfrow = c(4,3))


# analitycznie uzyskany rozkład X to 1/10 dla x = 0,2,...,9
hist(X, breaks = "FD", prob = TRUE, main = "Rozklad brzegowy X")

# analitycznie uzyskany rozkład a posteriori U w zależności od X to (?)
for (i in 1:(k+1))
{
  U_x[[i]] <- U_x[[i]][!is.na(U_x[[i]])]
  hist(U_x[[i]], breaks = "FD", prob = TRUE,
       main = sprintf("Rozklad a posteriori U dla X = %d", i-1))
}

U_apriori <- sapply(X, function (x) {sample(U_x[[x+1]], 1)})
hist(U_apriori, breaks = "FD", prob = TRUE, main = "Rozklad U|X")