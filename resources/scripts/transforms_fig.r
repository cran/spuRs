# transforms_fig.r

plot(c(-1.1, 2.9), c(0, 2), type="n", xlab="x", ylab="density at x (truncated at 2)")
lines(c(-1.1, -1, 1, 1, 2.9), c(0, 0, 1, 0, 0), lty=2)
e <- exp(1)
x <- seq(1/e, e, .01)
y <- (log(x)+1)/2/x
lines(c(-1.1, x, e, 2.9), c(0, y, 0, 0), lty=3)
x <- seq(.01, 1, .01)
y <- 1/2/sqrt(x)
lines(c(-1.1, 0, 0, x, 1, 2.9), c(0, 0, 4, y, 0, 0), lty=4)