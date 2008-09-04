f1 <- function(x) {
  # cos(x) - x
  fx <- cos(x) - x
  dfx <- -sin(x) - 1
  ddfx <- -cos(x)
  return(c(fx, dfx, ddfx))
}

f2 <- function(x) {
  # log(x) - exp(-x)
  fx <- log(x) - exp(-x)
  dfx <- 1/x + exp(-x)
  ddfx <- -1/x^2 - exp(-x)
  return(c(fx, dfx, ddfx))
}

f3 <- function(x) {
  # x^3 - x - 3
  fx <- x^3 - x - 3
  dfx <- 3*x^2 - 1
  ddfx <- 6*x
  return(c(fx, dfx, ddfx))
}

f4 <- function(x) {
  # x^3 - 7*x^2 + 14*x - 8
  fx <- x^3 - 7*x^2 + 14*x - 8
  dfx <- 3*x^2 - 14*x + 14
  ddfx <- 6*x - 14
  return(c(fx, dfx, ddfx))
}

f5 <- function(x) {
  # log(x)*exp(-x)
  fx <- log(x)*exp(-x)
  dfx <- exp(-x)/x - log(x)*exp(-x)
  ddfx <- -exp(-x)/x - exp(-x)/x^2 - exp(-x)/x + log(x)*exp(-x)
  return(c(fx, dfx, ddfx))
}


quadrootfinder <- function(ftn, x0, tol = 1e-6, max.iter = 100) {
  # do first iteration
  xold <- x0
  f.xold <- ftn(xold)
  a <- f.xold[3]/2
  b <- f.xold[2] - xold*f.xold[3]
  c <- f.xold[1] - xold*f.xold[2] + xold^2*f.xold[3]/2
  d <- b^2 - 4*a*c
  if (d <= 0) {
    xnew <- -b/2/a
  } else {
    xnew1 <- (-b + sqrt(d))/2/a
    xnew2 <- (-b - sqrt(d))/2/a
    if (abs(xold - xnew1) < abs(xold - xnew2)) {
      xnew <- xnew1
    } else {
      xnew <- xnew2
    }
  }
  iter <-  1
  cat("At iteration 1 value of x is:", xnew, "\n")

  # continue iterating until stopping conditions are met
  while ((abs(f.xold[1]) > tol) & (iter < max.iter)) {
    xold <- xnew;
    f.xold <- ftn(xold)
    a <- f.xold[3]/2
    b <- f.xold[2] - xold*f.xold[3]
    c <- f.xold[1] - xold*f.xold[2] + xold^2*f.xold[3]/2
    d <- b^2 - 4*a*c
    if (d <= 0) {
      xnew <- -b/2/a
    } else {
      xnew1 <- (-b + sqrt(d))/2/a
      xnew2 <- (-b - sqrt(d))/2/a
      if (abs(xold - xnew1) < abs(xold - xnew2)) {
        xnew <- xnew1
      } else {
        xnew <- xnew2
      }
    }
    iter <-  iter + 1
    cat("At iteration", iter, "value of x is:", xnew, "\n")
  }

  # output depends on success of algorithm
  if (abs(f.xold[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
    return(xnew)
  }
}