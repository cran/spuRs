SIRplot <- function(SIR, a = "?", b = "?") {
  # plot an SIR epidemic
  T = length(SIR[,1]) - 1
  N = sum(SIR[1,])
  par(mfrow = c(3, 1))
  plot(c(0, T), c(0, N), xlab = "t", ylab = "S[t]", type = "n",
    main = paste("SIR epidemic\n alpha = ", a, " beta = ", b))
  lines(0:T, SIR[,1])
  plot(c(0, T), c(0, N), xlab = "t", ylab = "I[t]", type = "n")
  lines(0:T, SIR[,2])
  plot(c(0, T), c(0, N), xlab = "t", ylab = "R[t]", type = "n")
  lines(0:T, SIR[,3])
  par(mfrow = c(1, 1))
}
