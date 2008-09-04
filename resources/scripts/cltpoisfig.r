postscript('cltpoisfig.ps', width=6, height=2)

par(mfrow=c(1,3))

plot(0:8, dpois(0:8, 3), type='h',
    xlab='', ylab='p.m.f. and p.d.f.', main='Poisson(3)', ps=10)
points(0:8, dpois(0:8, 3))
lines(seq(0, 8, 0.2), sapply(seq(0, 8, 0.2), dnorm, mean=3, sd=sqrt(3)))
    
plot(0:20, dpois(0:20, 8), type='h',
    xlab='', ylab='p.m.f. and p.d.f.', main='Poisson(8)', ps=10)
points(0:20, dpois(0:20, 8))
lines(seq(0, 20, 0.2), sapply(seq(0, 20, 0.2), dnorm, mean=8, sqrt(8)))

plot(10:30, dpois(10:30, 20), type='h',
    xlab='', ylab='p.m.f. and p.d.f.', main='Poisson(20)', ps=10)
points(10:30, dpois(10:30, 20))
lines(seq(10, 30, 0.2), sapply(seq(10, 30, 0.2), dnorm, mean=20, sd=sqrt(20)))

dev.off()