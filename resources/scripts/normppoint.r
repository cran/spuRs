postscript('normppoint.ps', width=6, height=6)

par(mfrow=c(1,1))

curve(dnorm, -4, 4, xlab='x', ylab='phi(x)')
polygon(c(seq(-4, -1.64, .02), -1.64), c(dnorm(seq(-4, -1.64, .02)), 0), col='grey90')
polygon(c(1.64, seq(1.64, 4, .02)), c(0, dnorm(seq(1.64, 4, .02))), col='grey90')
lines(c(1.64, 1.64), c(0, dnorm(1.64)), col='grey50')
lines(c(-1.64, -1.64), c(0, dnorm(-1.64)), col='grey50')
lines(c(-4, 4), c(0, 0))
text(-1.64,0,'z(0.05)', pos=3)
text(1.64,0,'z(0.95)', pos=3)
text(0, .1, 'area 0.90')
text(-3, .04, 'area 0.05')
text(3, .04, 'area 0.05')

dev.off()