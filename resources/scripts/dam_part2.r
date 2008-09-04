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


# input
A2 <- function(h) {
  if (h < 0) {
    return(0)
  } else if (h < 2) {
    return(100*h^2)
  } else {
    return(400*h - 400)
  }
}
h1 <- 5
hmax <- 10
v <- scan(file = "../data/catchment.txt") # volume of water falling into catchment on day t is v[t+1]
alpha <- 1
beta <- 0.05

# calculations
n <- length(v)
h <- rep(0, n+1) # height of dam at end of day t is h[t+1]
h[1] <- h1
for (i in 1:n) {
  h[i+1] <- height(h[i], hmax, v[i] - alpha - beta*A2(h[i]), A2)
}

# output
plot(1:(n+1), h, type = "l", xlab = 'day', ylab = 'level',
  main = 'Daily water level in a dam')
