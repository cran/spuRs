source('quadrature.r')
f <- function(x) exp(x^2/2)
c <- 1/quadrature(f, -1, 1, 1e-8)
xf <- function(x) x*exp(x^2/2)
mu <- quadrature(xf, -1, 1, 1e-8)*c
cat('mean of X is ', mu, '\n')
