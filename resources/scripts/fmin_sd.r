gsection <- function(ftn, x.l, x.r, x.m, tol = 1e-9) {
  # applies the golden section algorithm to minimise ftn
  # we assume that ftn is a function of a single variable
  # and that x.l < x.m < x.r and ftn(x.l), ftn(x.r) >= ftn(x.m)
  #
  # the algorithm iteratively refines x.l, x.r and x.m and terminates when
  # x.r - x.l <= tol, then returns x.m

  # golden ratio plus one
  gr1 <- 1 + (1 + sqrt(5))/2

  # successively refine x.l, x.r and x.m
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  f.m <- ftn(x.m)
  while ((x.r - x.l) > tol) {
    if ((x.r - x.m) > (x.m - x.l)) {
      y <- x.m + (x.r - x.m)/gr1
      f.y <- ftn(y)
      if (f.y <= f.m) {
        x.l <- x.m
        f.l <- f.m
        x.m <- y
        f.m <- f.y
      } else {
        x.r <- y
        f.r <- f.y
      }
    } else {
      y <- x.m - (x.m - x.l)/gr1
      f.y <- ftn(y)
      if (f.y <= f.m) {
        x.r <- x.m
        f.r <- f.m
        x.m <- y
        f.m <- f.y
      } else {
        x.l <- y
        f.l <- f.y
      }
    }
  }
  return(x.m)
}


line.search <- function(f, x, y, tol = 1e-9, a.max = 2^5) {
    # f is a real function that takes a vector of length d
    # x and y are vectors of length d; y != rep(0, d)
    # line.search uses gsection to find a > 0 such that g(a) = f(x - a*y) has
    #   a local minimum at a, within a tolerance of tol
    # if no local min is found then line.search uses tol or a.max for a
    # the value returned is x - a*y
    
    g <- function(a) return(f(x - a*y))
    
    # find a triple a.l < a.m < a.r such that
    # g(a.l) >= g(a.m) and g(a.m) <= g(a.r)
    # a.l
    a.l <- 0
    g.l <- g(a.l)
    # a.m
    a.m <- 1
    g.m <- g(a.m)
    while ((g.m > g.l) & (a.m > tol)) {
        a.m <- a.m/2
        g.m <- g(a.m)
    }
    # if a suitable a.m was not found then use tol for a
    if ((a.m <= tol) & (g.m > g.l)) return(x - tol*y)
    # a.r
    a.r <- 2*a.m
    g.r <- g(a.r)
    while ((g.m > g.r) & (a.r < a.max)) {
        a.m <- a.r
        g.m <- g.r
        a.r <- 2*a.m
        g.r <- g(a.r)
    }
    # if a suitable a.r was not found then use a.max for a
    if ((a.r >= a.max) & (g.m > g.r)) return(x - a.max*y)
    
    # apply golden-section algorithm to g to find a
    a <- gsection(g, a.l, a.r, a.m, tol)
    return(x - a*y)
}


fmin.sd <- function(f, grad.f, x0, tol = 1e-9, a.max = 2^5) {
    # find a local minimum of f near the point x0
    # let d = length(x0) then f is a function that takes
    #   a vector of length d and returns a real number
    # grad.f is a function that takes a vector of length d
    #   and returns a vector of length d, which should be
    #   the gradient vector of f
    # algorithm stops when successive approximations are within tol 
    #   in each co-ordinate
    # parameter a.max is passed on to line.search 

    x <- x0
    x.old <- x + 2*tol # so while loop executes at least once
    while (max(abs(x - x.old)) > tol) {
        x.old <- x
        x <- line.search(f, x, grad.f(x), tol, a.max)
    }
    return(x)
}
