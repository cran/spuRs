p_X <- function(x) 6/pi^2/x^2
E_n <- function(n) n - sum( (n - 1:(n-1)) * p_X(1:(n-1)) )
eps <- 1e-6
n <- 2
S <- p_X(1)
while (S < 1 - eps) {
  n <- n + 1
  S <- S + 6/pi^2/(n-1)^2
}
n
E_n(n)
E_n(2*n)