rpareto <- function(n, alpha, beta) beta*(runif(n)^(-1/alpha) - 1)
dpareto <- function(x, alpha, beta) alpha*beta^alpha/(x + beta)^(alpha + 1)

set.seed(1)
p <- rpareto(1000, 3, 100000)
hist(p, 20, freq=F, xlab="x", ylab="f(x)", main="Pareto(3, 100000) density")
p.max <- max(p)
x <- seq(0, p.max, p.max/100)
lines(x, dpareto(x, 3, 100000))