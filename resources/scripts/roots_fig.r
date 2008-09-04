f <- function(x) ((x - 4)^2 - 9)/(1+x/4)^2

curve(f, from = 0, to = 10)
abline(0, 0)
points(c(1,7), c(0,0))
text(c(1,7), c(0,0), c('root at x=1', 'root at x=7'), pos=3, offset=2)
