rho <- (1+sqrt(5))/2
plot(c(-0.4,rho), c(-0.4,1.4), asp=1, axes=F, type="n", xlab="", ylab="", mar=c(1, 1, 1 ,1),
main=expression(paste("Golden ratio: ", frac(b,a), " = ", frac(a,c), " = ", frac(1+sqrt(5),2))))
lines(c(0, rho, rho, 0, 0), c(0, 0, 1, 1, 0))
lines(c(1, 1), c(0, 1))
arrows(c(0, -0.2, 0, 1), c(-0.2, 0, 1.2, 1.2), c(rho, -0.2, 1, rho), c(-0.2, 1, 1.2, 1.2), code=3)
text(c(rho/2, -0.3, 0.5, 1+(rho-1)/2), c(-0.3, 0.5, 1.3, 1.3), c("b", "a", "a", "c"))


