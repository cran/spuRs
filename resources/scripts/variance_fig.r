opar <- par(mfrow=c(1,3))

x <- 0:8
p <- c(4, 3, 2, 1, 0, 1, 2, 3, 4)/20
plot(x, p, type="h", ylim=c(0,.5), xlab="x", ylab="p(x)",
  main=paste("mean =", sum(x*p), "and\n variance =", round(sum((x-sum(x*p))^2*p), 2)))

x <- 0:8
p <- rep(1/9, 9)
plot(x, p, type="h", ylim=c(0,.5), xlab="x", ylab="p(x)",
  main=paste("mean =", sum(x*p), "and\n variance =", round(sum((x-sum(x*p))^2*p), 2)))
  
x <- 0:8
p <- c(0:4, 3:0)/16
plot(x, p, type="h", ylim=c(0,.5), xlab="x", ylab="p(x)",
  main=paste("mean =", sum(x*p), "and\n variance =", round(sum((x-sum(x*p))^2*p), 2)))

#x <- 0:8
#p <- c(0, 0, 1, 4, 9, 4, 1, 0, 0)/19
#plot(x, p, type="h", ylim=c(0,.5), xlab="x", ylab="p(x)",
#  main=paste("mean =", sum(x*p), "and\n  variance =", round(sum((x-sum(x*p))^2*p), 2)))

par(opar)