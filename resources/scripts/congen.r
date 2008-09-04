cong.gen <- function(n, X0, A, B, m) {
  X <- rep(0, n+1)
  X[1] <- X0 %% m
  for (i in 1:n) {
    X[i+1] <- (A*X[i] + B) %% m
  }
  return(X)
}

# From Numerical Recipes in C
# A = 1664525; B = 1013904223; m = 2^32

# m = 64; A = 29; B = 17
# m = 64; A = 9; B = 1
# m = 64; A = 13; B = 0
# m = 64; A = 11; B = 0

# RANDU
m = 2^31; A = 65539; B = 0

X <- cong.gen(1000, 1, A, B, m)

par(mfrow=c(2,1))
hist((1+X)/(1+m), seq(0, 1, max(0.01, 1/(m+1))))
plot(X[1:(length(X)-1)], X[2:length(X)], xlab = "X[i]", ylab = "X[i+1]")
par(mfrow=c(1,1))