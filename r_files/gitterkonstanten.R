library("plotrix")
source("linearfit.R")
source("round.R")

lambda=632.8
#corrfac=1/63.21204

m1=c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
m2=c(-1,0,1,2)
m3=c(-2,-1,0,1,2)
m4=c(-2,-1,0,1,2)
m5=c(-2,-1,0,1,2)

sin1 =c(1.21,1.69,2.18,2.67,3.16,3.65,4.14,4.63,5.12,5.61,6.10)*10^(-2)-0.0365
ssin1=c(0.09,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.07)*10^(-2)
sin2 =c(5.61,7.45,9.30,11.15)*10^(-2)-0.0745
ssin2=c(0.06,0.06,0.06,0.06)*10^(-2)
sin3 =c(2.46,3.08,3.70,4.31,4.92)*10^(-2)-0.0370
ssin3=c(0.06,0.06,0.06,0.06,0.06)*10^(-2)
sin4 =c(1.85,3.08,3.69,4.30,5.53)*10^(-2)-0.0369
ssin4=c(0.06,0.06,0.06,0.06,0.06)*10^(-2)
sin5 =c(1.24,2.46,3.69,4.91,6.15)*10^(-2)-0.0369
ssin5=c(0.06,0.06,0.06,0.06,0.05)*10^(-2)

#sin1=c(-3.93,-3.17,-2.4,-1.62,-0.85,0,0.71,1.48,2.55,3.04,3.81)
#ssin1=c(0.17,0.14,0.14,0.14,0.14,0.10,0.13,0.14,0.14,0.14,0.15)
#sin2=c(-2.91,0,2.93,5.87)
#ssin2=c(0.14,0.1,0.14,0.13)
#sin3=c(-1.94,-0.97,0,0.98,1.95)
#ssin3=c(0.13,0.13,0.1,0.13,0.13)
#sin4=c(-2.91,-0.97,0,0.97,2.91)
#ssin4=c(0.13,0.14,0.1,0.14,0.14)
#sin5=c(-3.88,-1.94,0,1.93,3.89)
#ssin5=c(0.14,0.14,0.1,0.14,0.13)

plot(sin1,m1,pch=4,cex=0.6,bty="l",xlab="sin(theta)",ylab="m")
plotCI(sin1,m1,uiw=ssin1,err="x",pch=4,cex=0.6,add=TRUE)
fit1=linearfit(data.frame(x=sin1,y=m1))
plotlinear(fit1,c(sin1[1],sin1[length(sin1)]))
plotlindata(fit1)
grid()
slope=roundfunc(c(fit1[2],fit1[4]))
intercept=roundfunc(c(fit1[1],fit1[3]))
vorzeichen="+"
if(fit1[1]<0)
  vorzeichen=""
legend(-0.02,4,paste("m=(",slope[1],"+-",slope[2],")sin(theta) ",vorzeichen,intercept[1],"+-",intercept[2],sep=""))
plot(sin2,m2,pch=4,cex=0.6,bty="l",xlab="sin(theta)",ylab="m")
plotCI(sin2,m2,uiw=ssin2,err="x",pch=4,cex=0.6,add=TRUE)
fit2=linearfit(data.frame(x=sin2,y=m2))
plotlinear(fit2,c(sin2[1],sin2[length(sin2)]))
plotlindata(fit2)
grid()
slope=roundfunc(c(fit2[2],fit2[4]))
intercept=roundfunc(c(fit2[1],fit2[3]))
vorzeichen="+"
if(fit2[1]<0)
  vorzeichen=""
legend(-0.015,1.5,paste("m=(",slope[1],"+-",slope[2],")sin(theta) ",vorzeichen,intercept[1],"+-",intercept[2],sep=""))
plot(sin3,m3,pch=4,cex=0.6,bty="l",xlab="sin(theta)",ylab="m")
plotCI(sin3,m3,uiw=ssin3,err="x",pch=4,cex=0.6,add=TRUE)
fit3=linearfit(data.frame(x=sin3,y=m3))
plotlinear(fit3,c(sin3[1],sin3[length(sin3)]))
plotlindata(fit3)
grid()
slope=roundfunc(c(fit3[2],fit3[4]))
intercept=roundfunc(c(fit3[1],fit3[3]))
vorzeichen="+"
if(fit3[1]<0)
  vorzeichen=""
legend(-0.01,1.5,paste("m=(",slope[1],"+-",slope[2],")sin(theta) ",vorzeichen,intercept[1],"+-",intercept[2],sep=""))
plot(sin4,m4,pch=4,cex=0.6,bty="l",xlab="sin(theta)",ylab="m")
plotCI(sin4,m4,uiw=ssin4,err="x",pch=4,cex=0.6,add=TRUE)
fit4=linearfit(data.frame(x=sin4,y=m4))
plotlinear(fit4,c(sin4[1],sin4[length(sin4)]))
plotlindata(fit4)
grid()
slope=roundfunc(c(fit4[2],fit4[4]))
intercept=roundfunc(c(fit4[1],fit4[3]))
vorzeichen="+"
if(fit4[1]<0)
  vorzeichen=""
legend(-0.015,1.5,paste("m=(",slope[1],"+-",slope[2],")sin(theta) ",vorzeichen,intercept[1],"+-",intercept[2],sep=""))
plot(sin5,m5,pch=4,cex=0.6,bty="l",xlab="sin(theta)",ylab="m")
plotCI(sin5,m5,uiw=ssin5,err="x",pch=4,cex=0.6,add=TRUE)
fit5=linearfit(data.frame(x=sin5,y=m5))
plotlinear(fit5,c(sin5[1],sin5[length(sin5)]))
plotlindata(fit5)
grid()
slope=roundfunc(c(fit5[2],fit5[4]))
intercept=roundfunc(c(fit5[1],fit5[3]))
vorzeichen="+ "
if(fit5[1]<0)
  vorzeichen=""
legend(-0.02,1.5,paste("m=(",slope[1],"+-",slope[2],")sin(theta) ",vorzeichen,intercept[1],"+-",intercept[2],sep=""))

a=c(fit1[2],fit2[2],fit3[2],fit4[2],fit5[2])
aerr=c(fit1[4],fit2[4],fit3[4],fit4[4],fit5[4])

K=a*lambda
Kerr=aerr*lambda

#Vergrößerungen

Knm=K*10^(-9)
d=3*10^(-3)
m=c(5,2,2,2,2)
a=d*m/Knm
aerr=sqrt((0.05/3)^2+(Kerr/K)^2)*a