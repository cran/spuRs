# programme spuRs/resources/scripts/basic_mc.r
# date: 17 May 2006

f <- function(x) {
  return(x^3-7*x^2+1)
}

basic.mc <- function(ftn, a, b, n) {
  # Monte-Carlo Integration of ftn over the interval [a, b]
  # using a sample of size n
  s <- 0
  for (i in 1:n) {
    s <- s + ftn(runif(1,a,b))
  }
  I = s/n*(b-a)
  return(I)
}

basic.mc2 <- function(ftn, a, b, n) {
  # One line version of basic.mc
  # An example of bad programming because it is unclear
  return(mean(sapply(runif(n,a,b), ftn))*(b-a))
}

basic.mc3 <- function(ftn, a, b, n) {
  # Vectorised version of basic.mc
  # Returns a vector of successive estimates
  x <- sapply(runif(n, a, b), ftn)
  x.means <- cumsum(x)/(1:n)
  return(x.means*(b-a))
}

hit.miss3 <- function(ftn, a, b, f.min, f.max, n) {
  # Vectorised version of hit.miss
  # Returns a vector of successive estimates
  X <- runif(n, a, b)
  Y <- runif(n, f.min, f.max)
  Z <- (Y <= sapply(X, ftn))
  I <- (b - a)*f.min + (cumsum(Z)/(1:n))*(b - a)*(f.max - f.min)
  return(I)
}

# Compare the hit & miss and basic mc methods with correct integral
set.seed(1)
n <- 1000
I1 <- hit.miss3(f, 0, 1, -7, 2, n)
I2 <- basic.mc3(f, 0, 1, n)
plot(1:n, I1, type = "l", col = "purple", xlab = "sample size", ylab = "estimate")
lines(1:n, I2, col = "hot pink")
lines(c(1, n), c(-13/12, -13/12))
