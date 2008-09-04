# system parameters
D <- 1000
L <- 0.1
K <- 1000
p <- 100
h <- 100
s <- 200
# control parameters
q <- 146
r <- 115

plot(c(0, L, L, q/D), c(r, r-L*D, q+r-L*D, r), ylim=c(0, q+r-L*D), type="l", xlab="t", ylab="E I(t)")
text(L, 0, "L", pos=3)
text(q/D, 0, "q/D", pos=3)
abline(h=r, lty=2)
text(0, r, "r", pos=1, srt=90, offset=1)
abline(h=r-L*D, lty=2)
text(0, r-L*D, "m", pos=1, srt=90, offset=1)
abline(h=q+r-L*D, lty=2)
text(0, q+r-L*D, "q+m", pos=1, srt=90, offset=1)