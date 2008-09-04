# program spuRs/resources/scripts/SIR.r

# discrete SIR epidemic model

n.reps <- 20 # num times simulation is run

# model inputs are population size N, time T,
# infection rate a and removal rate b
N <- 1000
T <- 100
# a <- 0.01; b <- 0.1 # all get infected
# a <- 0.001; b <- 0.2 # occasionally epidemic doesn't start
# a <- 0.0005; b <- 0.2 # some small epidemics
a <- 0.0005; b <- 0.3 # balanced
# a <- 0.0005; b <- 0.4 # mostly small epidemics
# a <- 0.0005; b <- 0.5 # all small epidemics

# susceptibles in S, infected in I, removed in R
# row rep is for repetition rep, col i is for time i+1
S <- matrix(0, nrow = n.reps, ncol = T+1)
I <- matrix(0, nrow = n.reps, ncol = T+1)
R <- matrix(0, nrow = n.reps, ncol = T+1)

# main loop
for (rep in 1:n.reps) {
  S[rep, 1] <- N
  I[rep, 1] <- 1
  R[rep, 1] <- 0
  for (i in 1:T) {
    S[rep, i+1] <- rbinom(1, S[rep, i], (1 - a)^I[rep, i])
    R[rep, i+1] <- R[rep, i] + rbinom(1, I[rep, i], b)
    I[rep, i+1] <- I[rep, i] - R[rep, i+1] + R[rep, i] - S[rep, i+1] + S[rep, i]
  }
}

# plot results
par(mfrow = c(3, 1))
plot(c(0, T), c(0, N), xlab = "t", ylab = "S[t]", type = "n",
  main = paste("SIR epidemic\n alpha = ", a, " beta = ", b))
for (rep in 1:n.reps) {
  lines(0:T, S[rep,])
}
plot(c(0, T), c(0, N), xlab = "t", ylab = "I[t]", type = "n")
for (rep in 1:n.reps) {
  lines(0:T, I[rep,])
}
plot(c(0, T), c(0, N), xlab = "t", ylab = "R[t]", type = "n")
for (rep in 1:n.reps) {
  lines(0:T, R[rep,])
}
par(mfrow = c(1, 1))
