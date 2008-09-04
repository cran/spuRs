newton <- function(f3, x0, tol = 1e-9, n.max = 100, col = 'black') {
    # Newton's method for optimisation, starting at x0
    # f3 is a function that given x returns the list
    # {f(x), grad f(x), Hessian f(x)}, for some f

    x <- x0
    f3.x <- f3(x)
    n <- 0
    points(x[1], x[2], pch=19, col=col)
    while ((max(abs(f3.x[[2]])) > tol) & (n < n.max)) {
        x.old <- x
        x <- x - solve(f3.x[[3]], f3.x[[2]])
        f3.x <- f3(x)
        n <- n + 1
        lines(c(x.old[1], x[1]), c(x.old[2], x[2]), col=col)
        points(x[1], x[2], col=col)
    }
    if (n == n.max) {
        cat('newton failed to converge\n')
    } else {
        return(x)
    }
}

gamma.2.3 <- function(x) {
    # gamma(2,3) density
    if (x < 0) return(c(0, 0, 0))
    if (x == 0) return(c(0, 0, NaN))
    y <- exp(-2*x)
    return(list(4*x^2*y, 8*x*(1-x)*y, 8*(1-2*x^2)*y))
}

f3 <- function(x) {
  a <- x[1]^2/2 - x[2]^2/4
  b <- 2*x[1] - exp(x[2])
  f <- sin(a)*cos(b)
  f1 <- cos(a)*cos(b)*x[1] - sin(a)*sin(b)*2
  f2 <- -cos(a)*cos(b)*x[2]/2 + sin(a)*sin(b)*exp(x[2])
  f11 <- -sin(a)*cos(b)*(4 + x[1]^2) + cos(a)*cos(b) -
      cos(a)*sin(b)*4*x[1]
  f12 <- sin(a)*cos(b)*(x[1]*x[2]/2 + 2*exp(x[2])) +
      cos(a)*sin(b)*(x[1]*exp(x[2]) + x[2])
  f22 <- -sin(a)*cos(b)*(x[2]^2/4 + exp(2*x[2])) - cos(a)*cos(b)/2 -
      cos(a)*sin(b)*x[2]*exp(x[2]) + sin(a)*sin(b)*exp(x[2])
  return(list(f, c(f1, f2), matrix(c(f11, f12, f12, f22), 2, 2)))
}

f <- function(x) sin(x[1]^2/2 - x[2]^2/4)*cos(2*x[1] - exp(x[2]))

hess.appx <- function(x, ftn) {
  eps <- 1e-6
  H11 <- (ftn(x+c(eps,0)) - 2*ftn(x) + ftn(x-c(eps,0)))/eps/eps
  H12 <- (ftn(x+c(eps,eps)) - ftn(x+c(0,eps)) - ftn(x+c(eps,0)) + ftn(x))/eps/eps
  H22 <- (ftn(x+c(0,eps)) - 2*ftn(x) + ftn(x-c(0,eps)))/eps/eps
  return(matrix(c(H11, H12, H12, H22), 2, 2))
}

x <- seq(-.5, 3, .005)
y <- seq(-.8, 1.7, .005)
#x <- seq(-.5, 3, .05)
#y <- seq(-.8, 1.7, .05)
z <- matrix(0, length(x), length(y))
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i,j] <- f(c(x[i], y[j]))
  }
}
postscript('newton_fig.ps', width=6, height=6, colormodel='gray')
image(x, y, z, col=gray(seq(.3, 1, .05)),
    main='f(x,y)=sin(x^2/2-y^2/4)*cos(2*x-exp(y))')
newton(f3, c(1.8, 0.3), col='white')
newton(f3, c(1.7, 0.6))
newton(f3, c(1.7, 1.2))
dev.off()

