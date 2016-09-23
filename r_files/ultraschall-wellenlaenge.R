library("plotrix")
source("linearfit.R")

lambda=632.8*10^(-9)

m1=c(-3,-2,-1,0,1,2,3)

sin1 =c(-3.41,-2.24,-1.12,0,1.12,2.26,3.45)*10^(-3)
ssin1=c(0.05,0.04,0.04,0.02,0.04,0.05,0.08)*10^(-3)

plot(sin1,m1,pch=4,cex=0.6,bty="l",xlab="sin(theta)",ylab="m")
plotCI(sin1,m1,uiw=ssin1,err="x",pch=4,cex=0.6,add=TRUE)
fit1=linearfit(data.frame(x=sin1,y=m1))
plotlinear(fit1,c(sin1[1],sin1[length(sin1)]))
#plotlindata(fit1)
grid()

a=fit1[2]
aerr=fit1[4]

Lambda=a*lambda
Lambdaerr=aerr*lambda

#Vergrößerungen

d=3*10^(-3)
m=c(5,2,2,2,2)

cat(paste("Lambda = (",Lambda*10^6,"+-",Lambdaerr*10^6,") nm",sep=""))