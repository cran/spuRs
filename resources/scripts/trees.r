# program spuRs/resources/scripts/trees.r


CR <- function(t, theta) 
  theta[1]*(1 - exp(-theta[2]*t))^theta[3]

loss.L2 <- function(theta, age, vol) 
  sum((vol - sapply(age, CR, theta=theta))^2)

loss.L1 <- function(theta, age, vol) 
  sum(abs(vol - sapply(age, CR, theta=theta)))

trees <- read.csv('trees.csv')
tree <- trees[trees$ID=="1.3.11", 2:3]

theta0 <- c(1000, 0.1, 3)
theta.L2 <- optim(theta0, loss.L2, age=tree$Age, vol=tree$Vol)
theta.L1 <- optim(theta0, loss.L1, age=tree$Age, vol=tree$Vol)

plot(tree$Age, tree$Vol, type='p', xlab='Age', ylab='Volume',
  main='Tree 1.3.11')
Vol.L2 <- sapply(tree$Age, CR, theta=theta.L2$par)
lines(tree$Age, Vol.L2, col='blue')
Vol.L1 <- sapply(tree$Age, CR, theta=theta.L1$par)
lines(tree$Age, Vol.L1, col='blue', lty=2)
  
