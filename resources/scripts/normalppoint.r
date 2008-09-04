# program: spuRs/resources/scripts/normalppoint.r

simpson_n <- function(ftn, a, b, n = 100) {
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


phi <- function(x) {
  return(exp(-x^2/2)/sqrt(2*pi))
}


ppoint <- function(p, pdf = phi, z.min = -10, tol = 1e-9) {
  # calculate a percentage point
  #
  # p is assumed to be between 0 an 1
  # pdf is assumed to be a probability density function
  #
  # let F(x) be the integral of pdf from -infinity to x
  # we apply the Newton_Raphson algorithm to find z_p such that F(z_p) = p
  # that is, to find z_p such that F(z_p) - p = 0
  # note that the derivative of F(z) - p is just pdf(z)
  #
  # we approximate -infinity by z.min (that is we assume that the integral
  # of pdf from -infinity to z.min is negligible)

  # do first iteration
  x <- 0
  f.x <- simpson_n(pdf, z.min, x) - p
  # continue iterating until stopping conditions are met
  while (abs(f.x) > tol) {
    x <- x - f.x/pdf(x)
    f.x <- simpson_n(pdf, z.min, x) - p
  }
  return(x)
}
