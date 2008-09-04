fixedpoint_show <- function(ftn, x0, xmin = x0 - 1, xmax = x0 + 1, n.max) {
  # applies the fixed-point algorithm to find x such that ftn(x) == x
  # x0 is the starting point
  # subsequent iterations are plotted in the range [xmin, xmax]

  # plot the function
  x <- seq(xmin, xmax, (xmax - xmin)/200)
  fx <- sapply(x, ftn)
  plot(x, fx, type = "l", xlab = "", ylab = "", lwd = 2)
  lines(c(xmin, xmax), c(xmin, xmax))
  lines(c(xmin, xmax), c(0, 0))

  # do first iteration
  xold <- x0
  xnew <- ftn(xold)
  lines(c(xold, xold), c(xold, 0), lty = 2)
  lines(c(xold, xold, xnew), c(xold, xnew, xnew))
  lines(c(xnew, xnew), c(xnew, 0), lty = 2)
  n <- 1

  while (n < n.max) {
    xold <- xnew;
    xnew <- ftn(xold);
    lines(c(xold, xold, xnew), c(xold, xnew, xnew))
    lines(c(xnew, xnew), c(xnew, 0), lty = 2)
    n <- n + 1
  }

  return(invisible(xnew))
}

postscript(file="../graphics/fixedpoint_fig.ps", width=6, height=3.5)
par(mfrow=c(1,2))

f1 <- function(x) x^1.5
fixedpoint_show(f1, 1.5, 0, 3, 4)
title(main='Divergence', ylab=expression(paste('f(x)=', x^1.5)))

f2 <- function(x) x^0.75
fixedpoint_show(f2, 2.5, 0, 3, 20)
title(main='Convergence', ylab=expression(paste('f(x)=', x^0.75)))

dev.off()