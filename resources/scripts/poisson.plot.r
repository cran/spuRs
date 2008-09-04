# poisson(la) pmf

poisson.plot <- function(la, n) {
  op <- par(new = F)
  plot(c(0,n), c(0,1), type = 'n', xlab = 'x', ylab = 'P(X=x)')
  for (i in 0:n) {
    lines(c(i,i), c(0,dpois(i, la)))
    points(i, dpois(i, la))
  }
  title(paste('Poisson(', la, ')'))
  par(op)
}

par(mfrow = c(2,2))
poisson.plot(.5, 15)
poisson.plot(1, 15)
poisson.plot(2, 15)
poisson.plot(5, 15)
par(mfrow = c(1,1))