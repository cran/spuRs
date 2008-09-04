x<-seq(0,4,by=0.01)
f<- function (x) {
  if (x<1) {return(0)}
  if (x<3) {return((x-1)/2)}
  return(1)
}
y<-sapply(x,f)
plot(0:4,seq(0,1.2,0.3),type="n",
  main=paste("Inversion method for",expression(U(1,3))),
  lwd=2,ylim=c(0,1),xlab="X",ylab="U")

for (i in c(0.15,0.55)){
	xline1<-2*i+1
	xline2<-2*(i-0.1)+1
	y2<-i-0.1
	xv<-c(0,xline1,xline1,xline2,xline2,0)
	yv<-c(i,i,0,0,y2,y2)
  polygon(xv,yv,col="grey90", border=NA)
}

for (i in seq(0.05,1,by=0.05)){
  xline<-2*i+1
  lines(c(0,xline,xline), c(i,i,0),lty="dashed")
}
lines(x,y,lwd=2)
