f <- function(x) (exp(x-2) - 2)/(1 + exp(x-4))
par(mfrow=c(1,1))
curve(f, from=1, to=6, lwd=2)
abline(h=0)
x0 <- 5.5
x1 <- 4.5
slope <- (f(x1)-f(x0))/(x1-x0)
abline(f(x0)-x0*slope, slope)
lines(c(x0,x0), c(0,f(x0)), lty=2)
lines(c(x1,x1), c(0,f(x1)), lty=2)
text(x0, 0, expression(x[n-1]), pos=1)
text(x1, 0, expression(x[n]), pos=1)
x2 <- x0 - f(x0)/slope
lines(c(x2,x2), c(0,f(x2)), lty=2)
text(x2, 0, expression(x[n+1]), pos=3)