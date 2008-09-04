rpareto <- function(n, alpha, beta) beta*(runif(n)^(-1/alpha) - 1)

set.seed(2)
Z <- rep(0, 6)
Z[1] <- 1000000
for (i in 1:5) {
  if (Z[i] > 0) {
    Z[i+1] <- max(Z[i] + 5500000 - sum(rpareto(rbinom(1, 1000, 0.1), 3, 100000)), 0)
  } else {
    Z[i+1] <- 0
  }
}

plot(0:5, Z, type="o", xlab="Year", ylab="Assets", ylim=c(0,max(Z)))
