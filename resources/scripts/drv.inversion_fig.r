x <- -1:4
p <- dbinom(x, 3, 0.5)
F <- cumsum(p)
plot(x, F, type="s", xlab="X ~ binom(3, 0.5)", ylab="U ~ U(0,1)",
  main="simulating from a binom(3, 0.5) c.d.f.")
lines(c(-1,0), c(F[2], F[2]), lty=2)
lines(c(-1,1), c(F[3], F[3]), lty=2)
lines(c(-1,2), c(F[4], F[4]), lty=2)
lines(c(-1,3), c(F[5], F[5]), lty=2)
arrows(-.8,F[1],-.8,F[2],code=3, length=0.125)
arrows(-.8,F[2],-.8,F[3],code=3, length=0.125)
arrows(-.8,F[3],-.8,F[4],code=3, length=0.125)
arrows(-.8,F[4],-.8,F[5],code=3, length=0.125)
text(-.7,.95,paste("(",F[4],",",F[5],") mapped to 3", sep=""), pos=4)
text(-.7,.7,paste("(",F[3],",",F[4],") mapped to 2", sep=""), pos=4)
text(-.7,.3,paste("(",F[2],",",F[3],") mapped to 1", sep=""), pos=4)
text(-.7,.05,paste("(",F[1],",",F[2],") mapped to 0", sep=""), pos=4)


