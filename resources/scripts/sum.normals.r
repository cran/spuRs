z1 <- rnorm(10000, mean=1, sd=1)
z2 <- rnorm(10000, mean=1, sd=2)
z <- z1 + z2 # mean = 2, var = 1^2 + 2^2 = 5
hist(z, breaks=seq(-8, 12, .2), freq=F)
f <- function(x) exp(-(x-2)^2/10)/sqrt(10*pi) # N(2, 5) density 
x <- seq(-8, 12, .1)
f.x <- sapply(x, f)
lines(x, f.x)
