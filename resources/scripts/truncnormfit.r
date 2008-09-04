# likelihood function
ell <- function(theta, a, x) {
    mu <- theta[1]
    si <- theta[2]
    sum(log(dnorm(x, mu, si)) - log(1 - pnorm(a, mu, si)))
}

# inputs
mu <- 0
si <- 1
a <- -1

# generate sample
x <- rnorm(10000, mu, si)
x.small <- (x <= a)
while (sum(x.small) > 0) {
    x[x.small] <- rnorm(sum(x.small), mu, si)
    x.small <- (x <= a)
}

# maximise the likelihood
ell.optim <- optim(c(mu, si), ell, a = a, x = x, control = list(fnscale = -1))
cat("ML estimate of mu", ell.optim$par[1], "and sigma", ell.optim$par[2], "\n")
