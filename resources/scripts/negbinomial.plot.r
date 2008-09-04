# neg-binomial(r,p) pmf

negbinomial.plot <- function(r, p, n) {
  op <- par(new = F)
  plot(c(0,n), c(0,1), type = 'n', xlab = 'x', ylab = 'P(X=x)')
  for (i in 0:n) {
    lines(c(i,i), c(0,dnbinom(i, r, p)))
    points(i, dnbinom(i ,r, p))
  }
  title(paste('neg-binomial(', r, ',', p, ')'))
  par(op)
}

par(mfrow = c(2,2))
negbinomial.plot(2,.5, 20)
negbinomial.plot(3,.5, 20)
negbinomial.plot(10, .5, 20)
negbinomial.plot(10, .8, 20)
par(mfrow = c(1,1))