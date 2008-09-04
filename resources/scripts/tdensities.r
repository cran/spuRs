x <- seq(-6, 6, .1)
plot(x, sapply(x, dnorm), type='l', lwd=3, 
    xlab='x', ylab='density')
title('t densities with 1, 2, 4 and 10 d.f., and normal limit in bold')
for (i in c(1, 2, 4, 10)) {
  lines(x, sapply(x, dt, df=i))
}