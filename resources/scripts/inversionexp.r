x<-seq(0,4,by=0.01)
f<- function (x) {
return(1-exp(-1*x))
}
y<-sapply(x,f)
plot(0:4,seq(0,1.2,0.3),type="n",
  main=paste("Inversion method for",expression(exp(1))),
  lwd=2,ylim=c(0,1),xlab="X",ylab="U")

for (i in c(0.2,0.9)){
	xline1<--1*log(1-i)
	xline2<--1*log(1.05-i)
	y2<-i-0.05
	xv<-c(0,xline1,xline1,xline2,xline2,0)
	yv<-c(i,i,0,0,y2,y2)
polygon(xv,yv,col="grey90", border=NA)
}

for (i in seq(0.05,0.95,by=0.05)){
  xline<--1*log(1-i)
  lines(c(0,xline,xline), c(i,i,0),lty="dashed")
}
lines(c(0,4),c(1,1),lty="dashed")
lines(x,y,lwd=2)
