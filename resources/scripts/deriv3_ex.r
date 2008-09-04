Df <- deriv3(z ~ sin(x^2/2 - y^2/4)*cos(2*x - exp(y)), c('x', 'y'), func=T)

f3 <- function(x) {
  Dfx <- Df(x[1], x[2])
  f <- Dfx[1]
  gradf <- attr(Dfx, 'gradient')[1,]
  hessf <- attr(Dfx, 'hessian')[1,,]
  return(list(f, gradf, hessf))
}
