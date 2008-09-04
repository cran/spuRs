# draw a pmf and cdf
par(mfcol=c(2,1))

pX <- c((0:9)/45, 0)
plot(0:10, pX, type="h", xlab="x", ylab="p(x)")

FX <- cumsum(pX)
plot(0:10, FX, type="s", xlab="x", ylab="F(x)")

# draw a pdf and cdf
X11()
par(mfcol=c(2,1))
x <- seq(0, 10, .1)
fX <- x/50
plot(x, fX, type="l", xlab="x", ylab="f(x)")

FX <- x^2/100
plot(x, FX, type="l", xlab="x", ylab="F(x)")