simpson <- function(ftn, a, b, n = 100) {
  # this function approximates the integral from a to b of ftn
  # using Simpson's rule with n subdivisions
  # ftn is a function of a single variable
  # and we assume a < b and n is an even integer >= 4
  n <- max(c(2*(n %/% 2), 4))
  h <- (b-a)/n
  x.vec1 <- seq(a+h, b-h, by = 2*h)
  x.vec2 <- seq(a+2*h, b-2*h, by = 2*h)
  f.vec1 <- sapply(x.vec1, ftn)
  f.vec2 <- sapply(x.vec2, ftn)
  S <- h/3*(ftn(a) + ftn(b) + 4*sum(f.vec1) + 2*sum(f.vec2))
  return(S)
}


bisection <- function(ftn, x.l, x.r, tol = 1e-9, max.iter = 100) {
  # applies the bisection algorithm to find x such that ftn(x) == x
  # we assume that ftn is a function of a single variable
  #
  # x.l and x.r must bracket the fixed point, that is
  # x.l < x.r and ftn(x.l) * ftn(x.r) < 0
  #
  # the algorithm iteratively refines x.l and x.r and terminates when
  # x.r - x.l <= tol or the number of iterations exceeds max.iter

  # check inputs
  if (x.l > x.r) {
    cat("error: x.l > x.r \n")
    return(NULL)
  } else if (x.l == x.r) {
    cat("error: x.l == x.r \n")
    return(NULL)
  }
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  if (f.l == 0) {
    return(x.l)
  } else if (f.r == 0) {
    return(x.r)
  } else if (f.l * f.r > 0) {
    cat("error: ftn(x.l) * ftn(x.r) > 0 \n")
    return(NULL)
  }

  # sucessively refine x.l and x.r
  n <- 0
  while (((x.r - x.l) > tol) & (n < max.iter)) {
    x.m <- (x.l + x.r)/2
    f.m <- ftn(x.m)
    if (f.m == 0) {
      return(x.m)
    } else if (f.l * f.m < 0) {
      x.r <- x.m
      f.r <- f.m
    } else {
      x.l <- x.m
      f.l <- f.m
    }
    n <- n + 1
    # cat("at iteration", n, "the root lies between", x.l, "and", x.r, "\n")
  }

  # return (approximate) root or report failure to converge
  if ((x.r - x.l) <= tol) {
    return(x.l)
  } else {
    cat("max iterations reached without convergence \n")
    return(NULL)
  }
}


volume <- function(h, hmax, ftn) {
  # volume of water in dam at level h
  # hmax is max level and ftn(u) gives cross-sectional area at level u
  if (h < 0) {
    return(0)
  } else if (h > hmax) {
    return(simpson(ftn, 0, hmax))
  } else {
    return(simpson(ftn, 0, h))
  }
}


height0 <- function(h, hmax, v, ftn) {
  # new level of water in dam when initial level is h and volume changes by v
  # hmax is max level and ftn(u) gives cross-sectional area at level u
  vmax = volume(hmax, hmax, ftn)
  h = min(c(h, hmax))
  h = max(c(h, 0))
  v_h = volume(h, hmax, ftn)
  
  if (v_h + v >= vmax) {
    return(hmax)
  } else if (v_h + v <= 0) {
    return(0)
  } else {
    f <- function(u) {return(volume(u, hmax, ftn) - v_h - v)}
    u <- bisection(f, 0, hmax, tol = 1e-6)
    return(u)
  }
}


height <- function(h, hmax, v, ftn) {
  # new level of water in dam when initial level is h and volume changes by v
  # hmax is max level and ftn(u) gives cross-sectional area at level u
  vmax = volume(hmax, hmax, ftn)
  h = min(c(h, hmax))
  h = max(c(h, 0))
  v_h = volume(h, hmax, ftn)

  if (v_h + v >= vmax) {
    return(hmax)
  } else if (v_h + v <= 0) {
    return(0)
  } else {
    x.l <- 0
    x.r <- hmax
    f.l <- volume(x.l, hmax, ftn) - v_h - v
    f.r <- volume(x.r, hmax, ftn) - v_h - v
    # sucessively refine x.l and x.r
    while ((x.r - x.l) > 1e-6) {
      x.m <- (x.l + x.r)/2
      f.m <- volume(x.m, hmax, ftn) - v_h - v
      if (f.m == 0) {
        return(x.m)
      } else if (f.l * f.m < 0) {
        x.r <- x.m
        f.r <- f.m
      } else {
        x.l <- x.m
        f.l <- f.m
      }
    }
    return(x.l)
  }
}


A <- function(h) {return(h)}
h <- c(0, 2, 4, 1, 1)
v <- c(1, 1, 1, 0.1, -0.1)
for (i in 1:length(h)) {
  cat('h =', h[i], ' v =', v[i], ' height(h, 4, v, A) =', height(h[i], 4, v[i], A),
    ' sqrt(h^2 + 2*v) =', sqrt(h[i]^2 + 2*v[i]), '\n')
}
  


  