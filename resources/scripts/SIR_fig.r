rm(list=ls())

source("../scripts/SIRsim.r")

SIRplot <- function(SIR, a = "?", b = "?") {
  # plot an SIR epidemic
  T = length(SIR[,1]) - 1
  N = sum(SIR[1,])
  x <- par()$mar
  par(mar=c(1,4,3,2))
  plot(c(0, T), c(0, N), xlab = "", ylab = "S[t]", type = "n",
    main = paste("alpha = ", a, " beta = ", b))
  lines(0:T, SIR[,1])
  par(mar=c(2,4,2,2))
  plot(c(0, T), c(0, N), xlab = "", ylab = "I[t]", type = "n")
  lines(0:T, SIR[,2])
  par(mar=c(3,4,1,2))
  plot(c(0, T), c(0, N), xlab = "", ylab = "R[t]", type = "n")
  lines(0:T, SIR[,3])
  par(mar=x)
}

postscript(file="../graphics/SIR.ps", width=4.4, height=6)

layout(matrix(c(1,2,3,7,8,9,4,5,6,10,11,12),6,2), widths=c(2, 2), heights=rep(1, 6))

set.seed(2)
SIR <- SIRsim(0.0005, 0.1, 1000, 100)
SIRplot(SIR, 0.0005, 0.1)


set.seed(2)
SIR <- SIRsim(0.0005, 0.2, 1000, 100)
SIRplot(SIR, 0.0005, 0.2)

set.seed(4)
SIR <- SIRsim(0.0005, 0.3, 1000, 100)
SIRplot(SIR, 0.0005, 0.3)

set.seed(4)
SIR <- SIRsim(0.0005, 0.4, 1000, 100)
SIRplot(SIR, 0.0005, 0.4)

dev.off()
