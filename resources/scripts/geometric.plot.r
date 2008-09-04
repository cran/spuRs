# geometric(p) pmf

geometric.plot <- function(p, xmax=20) {
  op <- par(new = F)
  plot(c(0,xmax), c(0,1), type = 'n', xlab = 'x', ylab = 'P(X=x)')
  for (i in 0:xmax) {
    lines(c(i,i), c(0,dgeom(i, p)))
    points(i, dgeom(i, p))
  }
  title(paste('geometric(', p, ')'))
  par(op)
}

par(mfrow = c(2,2))
geometric.plot(.2)
geometric.plot(.4)
geometric.plot(.6)
geometric.plot(.8)
