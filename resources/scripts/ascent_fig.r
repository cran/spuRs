
ascent <- function(f, grad.f, x0, tol = 1e-9, n.max = 100) {
    # steepest ascent algorithm
    # find a local max of f starting at x0
    # function grad.f is the gradient of f
    
    x <- x0
    points(x[1], x[2], pch=19)
    x.old <- x
    x <- line.search(f, x, grad.f(x))
    n <- 1
    while ((f(x) - f(x.old) > tol) & (n < n.max)) {
        lines(c(x.old[1], x[1]), c(x.old[2], x[2]))
        points(x[1], x[2])
        x.old <- x
        x <- line.search(f, x, grad.f(x))
        n <- n + 1
    }
    return(x)
}

f <- function(x) sin(x[1]^2/2 - x[2]^2/4)*cos(2*x[1] - exp(x[2]))
gradf <- function(x) {
  f1 <- cos(x[1]^2/2 - x[2]^2/4)*x[1]*cos(2*x[1] - exp(x[2])) -
      sin(x[1]^2/2 - x[2]^2/4)*sin(2*x[1] - exp(x[2]))*2
  f2 <- -cos(x[1]^2/2 - x[2]^2/4)*x[2]/2*cos(2*x[1] - exp(x[2])) +
      sin(x[1]^2/2 - x[2]^2/4)*sin(2*x[1] - exp(x[2]))*exp(x[2])
  return(c(f1, f2))
}

x <- seq(-0.5, 3, .005)
y <- seq(-0.5, 2, .005)
z <- matrix(0, length(x), length(y))
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i,j] <- f(c(x[i], y[j]))
  }
}
postscript('ascent_fig1.ps', width=6, height=6, colormodel='gray')
image(x, y, z, col=gray(seq(.3, 1, .05)),
    main='f(x,y)=sin(x^2/2-y^2/4)*cos(2*x-exp(y))')
ascent(f, gradf, c(0.1, 0.3))
ascent(f, gradf, c(0, 0.5))
dev.off()

x2 <- seq(-0.5, 3, .1)
y2 <- seq(-0.5, 2, .1)
xyz <- data.frame(matrix(0, length(x2)*length(y2), 3))
names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x2)) {
  for (j in 1:length(y2)) {
    n <- n + 1
    ix <- which(abs(x - x2[i]) < 0.001)
    jy <- which(abs(y - y2[j]) < 0.001)
    xyz[n,] <- c(x2[i], y2[j], z[ix,jy])
  }
}
library(lattice)
postscript('ascent_fig2.ps', width=6, height=6)
print(wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE), zlab='f(x,y)', drape=T))
dev.off()
