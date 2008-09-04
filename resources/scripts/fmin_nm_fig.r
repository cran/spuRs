fmin.nm <- function(f, grad.f, hess.f, x0, tol = 1e-9) {
    x <- x0
    x.old <- x + 2*tol # so while loop executes at least once
    points(x[1], x[2], pch=19)
    while (max(abs(x - x.old)) > tol) {
        x.old <- x
        x <- x - solve(hess.f(x), grad.f(x))
        lines(c(x.old[1], x[1]), c(x.old[2], x[2]))
        points(x[1], x[2])
    }
    return(x)
}

f <- function(x) -sin(x[1]^2/2 - x[2]^2/4)*cos(2*x[1] - exp(x[2]))

gradf <- function(x) {
  a <- x[1]^2/2 - x[2]^2/4
  b <- 2*x[1] - exp(x[2])
  f1 <- -cos(a)*cos(b)*x[1] + sin(a)*sin(b)*2
  f2 <- cos(a)*cos(b)*x[2]/2 - sin(a)*sin(b)*exp(x[2])
  return(c(f1, f2))
}

hessf <- function(x) {
  a <- x[1]^2/2 - x[2]^2/4
  b <- 2*x[1] - exp(x[2])
  h11 <- sin(a)*cos(b)*(4 + x[1]^2) - cos(a)*cos(b) + cos(a)*sin(b)*4*x[1] 
  h12 <- -sin(a)*cos(b)*(x[1]*x[2]/2 + 2*exp(x[2])) - 
      cos(a)*sin(b)*(x[1]*exp(x[2]) + x[2])
  h22 <- sin(a)*cos(b)*(x[2]^2/4 + exp(2*x[2])) + cos(a)*cos(b)/2 + 
      cos(a)*sin(b)*x[2]*exp(x[2]) - sin(a)*sin(b)*exp(x[2])
  return(matrix(c(h11, h12, h12, h22), 2, 2))
}

hess.appx <- function(x, ftn) {
  eps <- 1e-6
  H11 <- (ftn(x+c(eps,0)) - 2*ftn(x) + ftn(x-c(eps,0)))/eps/eps
  H12 <- (ftn(x+c(eps,eps)) - ftn(x+c(0,eps)) - ftn(x+c(eps,0)) + ftn(x))/eps/eps
  H22 <- (ftn(x+c(0,eps)) - 2*ftn(x) + ftn(x-c(0,eps)))/eps/eps
  return(matrix(c(H11, H12, H12, H22), 2, 2))
}

x <- seq(-.5, 3, .005)
y <- seq(-.8, 1.7, .005)
z <- matrix(0, length(x), length(y))
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i,j] <- f(c(x[i], y[j]))
  }
}
postscript('fmin_nm_fig.ps', width=6, height=6, colormodel='gray')
image(x, y, z, col=gray(seq(.3, 1, .05)),
    main='f(x,y)=-sin(x^2/2-y^2/4)*cos(2*x-exp(y))')
fmin.nm(f, gradf, hessf, c(1.8, 0.3))
fmin.nm(f, gradf, hessf, c(1.7, 0.6))
fmin.nm(f, gradf, hessf, c(1.7, 1.2))
dev.off()

