postscript('cltbinfig.ps', width=6, height=2)

par(mfrow=c(1,3))

plot(0:4, dbinom(0:4, 4, 0.5), type='h',
    xlab='', ylab='p.m.f. and p.d.f.', main='binomial(4, 0.5)', ps=10)
points(0:4, dbinom(0:4, 4, 0.5))
lines(seq(0, 4, 0.2), sapply(seq(0, 4, 0.2), dnorm, mean=2, sd=1))
    
plot(0:16, dbinom(0:16, 16, 0.5), type='h',
    xlab='', ylab='p.m.f. and p.d.f.', main='binomial(16, 0.5)', ps=10)
points(0:16, dbinom(0:16, 16, 0.5))
lines(seq(0, 16, 0.2), sapply(seq(0, 16, 0.2), dnorm, mean=8, sd=2))

plot(20:44, dbinom(20:44, 64, 0.5), type='h',
    xlab='', ylab='p.m.f. and p.d.f.', main='binomial(64, 0.5)', ps=10)
points(20:44, dbinom(20:44, 64, 0.5))
lines(seq(20, 44, 0.2), sapply(seq(20, 44, 0.2), dnorm, mean=32, sd=4))

dev.off()