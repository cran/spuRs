# binomial(n,p) pmf

binomial.plot <- function(n, p) {
  op <- par(new = F)
  plot(c(0,n), c(0,1), type = 'n', xlab = 'x', ylab = 'P(X=x)')
  for (i in 0:n) {
    lines(c(i,i), c(0,dbinom(i,n, p)))
    points(i, dbinom(i,n, p))
  }
  title(paste('binomial(', n, ',', p, ')'))
  par(op)
}

par(mfrow = c(2,2))
binomial.plot(10,.5)
binomial.plot(20,.5)
binomial.plot(10,.2)
binomial.plot(10,.8)