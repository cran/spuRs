set.seed(21)

x<-seq(-1,5,by=0.01)
f<- function (x) {
    if(x<0){return(0)}
    if(x>4){return(0)}
    return((3/32)*(2*x^2-4*x+4-.25*x^3))
}
y<-sapply(x,f)
plot(x,xlim=c(-1,5),ylim=c(0,0.6),type="n",
    main="",lwd=2,xlab="x",ylab="pdf")

polygon(c(1,1,1.2,1.2),c(0,f(1),f(1.2),0),col="grey90", border=NA)
lines(c(1,1),c(0,f(1)),lty="dashed")
lines(c(1.2,1.2),c(0,f(1.2)),lty="dashed")

lines(c(0,0),c(0,0.5),lty="dashed")
lines(c(4,4),c(0,0.5),lty="dashed")
lines(c(0,4),c(0.5,0.5),lty="dashed")
lines(c(0,4),c(0,0),lty="dashed")
lines(x,y,lwd=2)
text(c(1,1.2),c(0,0),c("a","b"),pos=1,offset=0.25)
for (i in 1:80){
    xc<-runif(1,max=4)
    yc<-runif(1,max=f(xc))
    points(xc,yc,type="p")
}
