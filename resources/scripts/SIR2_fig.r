rm(list=ls())

source("../scripts/SIRsim.r")

SIRlistplot <- function(SIRlist, T, N, a = "?", b = "?") {
  # plot an SIR epidemic
  x <- par()$mar
  par(mar=c(1,4,3,2))
  plot(c(0, T), c(0, N), xlab = "", ylab = "S[t]", type = "n",
    main = paste("alpha = ", a, " beta = ", b))
  for (i in 1:length(SIRlist)) {
    lines(0:T, SIRlist[[i]][,1])
  }
  par(mar=c(2,4,2,2))
  plot(c(0, T), c(0, N), xlab = "", ylab = "I[t]", type = "n")
  for (i in 1:length(SIRlist)) {
    lines(0:T, SIRlist[[i]][,2])
  }
  par(mar=c(3,4,1,2))
  plot(c(0, T), c(0, N), xlab = "", ylab = "R[t]", type = "n")
  for (i in 1:length(SIRlist)) {
    lines(0:T, SIRlist[[i]][,3])
  }
  par(mar=x)
}

set.seed(6)

postscript(file="../graphics/SIR2.ps", width=4, height=4)

layout(matrix(c(1,2,3),3,1), widths=c(2), heights=rep(1, 3))
SIRlist <- list()
for (i in 1:20) {
  SIRlist[[i]] <- SIRsim(0.0005, 0.3, 1000, 100)
}
SIRlistplot(SIRlist, 100, 1000, 0.0005, 0.3)

dev.off()
