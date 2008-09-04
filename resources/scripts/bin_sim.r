bin.pmf <- function(x, n, p) {
  # binomial probability mass function
  # returns P(X = x) for X ~ bin(n, p)
  p.x <- (1-p)^n
  if (x > 0) {
    for (y in 1:x) {
      p.x <- p.x*(n-y+1)/y*p/(1-p)
    }
  }
  return(p.x)
}

# compare with built in function
show(bin.pmf(5, 10, .4) - dbinom(5, 10, .4))

bin.cdf <- function(x, n, p) {
  # binomial cumulative distribution function
  # returns P(X <= x) for X ~ bin(n, p)
  p.x <- (1-p)^n
  F.x <- p.x
  if (x > 0) {
    for (y in 1:x) {
      p.x <- p.x*(n-y+1)/y*p/(1-p)
      F.x <- F.x + p.x
    }
  }
  return(F.x)
}
    
# compare with built in function
show(bin.cdf(5, 10, .4) - pbinom(5, 10, .4))

# bin.pmf and bin.cdf will suffer from rounding error for large n
# for large n, (1-p)^n will get rounded to zero
x <- 1; n <- 0
while (x > 0) {x <- x*0.1; n <- n+1; cat(n, "\n")}

bin.rnd1 <- function(n, p) {
  # simulate a bin(n, p) random variable
  # using bin.cdf to calculate binomial probabilities
  u <- runif(1);                  # cat("u =", u, "\n")
  x <- 0;                         # cat("F(", x, ")= ", bin.cdf(x, n, p), "\n", sep = "")
  while (bin.cdf(x, n, p) < u) {
    x <- x + 1;                   # cat("F(", x, ")= ", bin.cdf(x, n, p), "\n", sep = "")
  }
  return(x)
}

bin.rnd2 <- function(n, p) {
  # simulate a bin(n, p) random variable
  # calculating binomial probabilities on the fly
  u <- runif(1);                  # cat("u =", u, "\n")
  x <- 0
  p.x <- (1-p)^n
  F.x <- p.x;                     # cat("F(", x, ")= ", F.x, "\n", sep = "")
  while (F.x < u) {
    x <- x + 1
    p.x <- p.x*(n-x+1)/x*p/(1-p)
    F.x <- F.x + p.x;             # cat("F(", x, ")= ", F.x, "\n", sep = "")
  }
  return(x)
}
    
bin.rnd3 <- function(n, p) {
  # simulate a bin(n, p) random variable using n indept Bernoulli trials
  return(sum(runif(n) < p))
}

bin.rnd.test <- function(n.reps, n, p) {
  # test bin.rnd2(n, p) by generating a sample of size n.reps
  # and comparing the empirical pmf with the theoretical pmf

  # count[x+1] is number of times x is observed in the sample
  count <- rep(0, n+1)
  for (i in 1:n.reps) {
    x <- bin.rnd2(n, p)
    count[x+1] <- count[x+1] + 1
  }
  
  # print a table of observed frequencies and theoretical probabilities
  cat('  x f_hat(x) f(x)\n')
  for (j in 0:n) {
    cat(format(j, width = 3), 
      format(count[j+1]/n.reps, nsmall = 6, scientific = FALSE), 
      format(bin.pmf(j, n, p), nsmall = 6, scientific = FALSE), '\n')
  }
  
  # plot the observed frequencies against the theoretical probabilities
  plot(0:n, count/n.reps, type = "p", xlab = "x", ylab = "P(X=x)", 
    main = paste("Binomial(", n, ",", p, ") pmf (lines) and estimate (points)\n", 
    "sample size = ", n.reps, sep = ""))
  for (j in 0:n) {
    lines(c(j, j), c(0, bin.pmf(j, n, p)))
  }
}

bin.rnd.test(10000, 10, .4)
