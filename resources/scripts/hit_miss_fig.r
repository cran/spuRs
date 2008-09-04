# hit and miss

plot(c(-1,5), c(-0.1,1.1), type="n", xlab='x', ylab='y=f(x)')
lines(c(-1,5),c(0,0))
lines(c(-1,5),c(1,1))
lines(c(0,0),c(-0.1,1.1))
lines(c(4,4),c(-0.1,1.1))
x <- seq(0,4,.05)
y <- 2*x*exp(-x)
lines(x,y)
for (i in 1:length(x)) lines(c(x[i], x[i]),c(0,y[i]))
title('f(x) = 2*x*exp(-x)')
