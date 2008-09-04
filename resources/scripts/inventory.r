rm(list=ls())
# D = 1000
# L = 0.1
# K = 1000
# p = 100
# h = 100
# s = 200
source("../scripts/simpson2.r")
f <- function(x) exp(-(x-100)^2/200)/sqrt(200*pi)
xf <- function(x) (x-r)*exp(-(x-100)^2/200)/sqrt(200*pi)
F <- function(x) {
  if (x > 100) return(0.5 + simpson(f, 100, x))
  else if (x < 100) return(0.5 - simpson(f, x, 100))
  else return(0.5)
}
source("../scripts/newtonraphson.r")
g <- function(r) c(F(r) - 0.95, f(r))
r <- newtonraphson(g, 100) # r <- 116
q <- sqrt(20000) # q <- 141

n <- function(r) sqrt(50/pi)*exp(-(r - 100)^2/200) - (r - 100)*(1 - F(r))
n.check <- function(r) {
  c <- 200
  if (r < c) {
    return( simpson(xf, r, c) )
  } else {
    return( 0 )
  }
}
n(r)
n.check(r)

G <- function(x) {
  q <- x[1]
  r <- x[2]
  A <- matrix(c(-1, 0, 0, 1), 2, 2)/50000
  return( A %*% c(100*q^2 - 2000*(1000 + 200*n(r)),
                  (1 - F(r))*200000 - 100*q) + c(q, r) )
}

tol <- 1e-3
x <- c(141, 116)
x.diff <- 1
while (x.diff > tol) {
  x.old <- x
  x <- G(x)
  x.diff <- sum(abs(x - x.old))
}
# x <- c(146, 115)

alpha <- 0.5 + simpson(f, 100, 115) # alpha <- 0.933

cqr <- function(q, r) {
  D <- 1000
  L <- 0.1
  K <- 1000
  p <- 100
  h <- 100
  s <- 200
  h*(r - L*D + q/2) + K*D/q + p*D + s*D*n(r)/q
}

cat("c(141, 116) =", cqr(141, 116), "\n") # 116071.9
cat("c(146, 115) =", cqr(146, 115), "\n") # 116050.8
