f <- function(x) max(
    -(sum(x^2)-2)*(sum(x^2)-1)*sum(x^2)*(sum(x^2)+1)*(sum(x^2)+2)*
    (2 - sin(x[1]^2 - x[2]^2)*cos(x[1] - exp(x[2]))), -3)

x <- seq(-1.5, 1.5, .05)
y <- seq(-1.5, 1.5, .05)
z <- matrix(0, length(x), length(y))
xyz <- data.frame(matrix(0, length(x)*length(y), 3))
names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i,j] <- f(c(x[i], y[j]))
    n <- n + 1
    xyz[n,] <- c(x[i], y[j], z[i, j])
  }
}

#image(x, y, z, col=gray(seq(.3, 1, .01)))
#windows()
library(lattice)
postscript('globalmax.ps', width=6, height=6)
print(wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE), pretty=T,
    zlab='f(x,y)', drape=T, screen=list(z = 20, x = -30, y = 3)))
dev.off()
