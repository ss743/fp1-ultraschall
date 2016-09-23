source("besselsquare.R")

peaksm3=c(-5.28,-5.40,-5.47,-5.37,-5.39,-5.54,-5.70,-5.70,-5.37,-5.23,-5.46,-5.28)*10^(-5)
peaksm2=c(-3.45,-3.57,-3.64,-3.54,-3.56,-3.71,-3.87,-3.87,-3.54,-3.54,-3.65,-3.42)*10^(-5)
peaksm1=c(-1.67,-1.79,-1.86,-1.76,-1.78,-1.93,-2.09,-2.09,-1.75,-1.65,-1.86,-1.65)*10^(-5)
peaks0=c(0,0,0,0,0,0,0,0,0,0,0,0)
peaksp1=c(1.97,1.85,1.78,1.88,1.86,1.71,1.55,1.55,1.87,2.06,1.83,1.90)*10^(-5)
peaksp2=c(3.86,3.74,3.69,3.77,3.75,3.60,3.44,3.44,3.77,3.98,3.78,3.70)*10^(-5)
peaksp3=c(5.59,5.47,5.40,5.50,5.48,5.33,5.17,5.17,5.50,5.64,5.41,5.59)*10^(-5)

U=c(0.00,0.88,1.73,2.43,3.20,4.20,5.10,6.08,7.04,8.01,8.97,9.69)

peakm3=-5.4*10^(-5)
peakm2=-3.57*10^(-5)
peakm1=-1.79*10^(-5)
peak0=0
peakp1=1.85*10^(-5)
peakp2=3.74*10^(-5)
peakp3=5.47*10^(-5)

ypeaksm3=c()
ypeaksm2=c()
ypeaksm1=c()
ypeaks0=c()
ypeaksp1=c()
ypeaksp2=c()
ypeaksp3=c()

for(i in 1:12){
  
  ypeaksm3[i]=gausfunction(fits[[i]],peaksm3[i])
  ypeaksm2[i]=gausfunction(fits[[i]],peaksm2[i])
  ypeaksm1[i]=gausfunction(fits[[i]],peaksm1[i])
  ypeaks0[i] =gausfunction(fits[[i]],peaks0[i])
  ypeaksp1[i]=gausfunction(fits[[i]],peaksp1[i])
  ypeaksp2[i]=gausfunction(fits[[i]],peaksp2[i])
  ypeaksp3[i]=gausfunction(fits[[i]],peaksp3[i])
  
}

normfactor=ypeaks0[1]

ypeaksm3=ypeaksm3/normfactor
ypeaksm2=ypeaksm2/normfactor
ypeaksm1=ypeaksm1/normfactor
ypeaks0=ypeaks0/normfactor
ypeaksp1=ypeaksp1/normfactor
ypeaksp2=ypeaksp2/normfactor
ypeaksp3=ypeaksp3/normfactor

ylim=c(min(c(ypeaksm3,ypeaksm2,ypeaksm1,ypeaks0,ypeaksp1,ypeaksp2,ypeaksp2)),max(c(ypeaksm3,ypeaksm2,ypeaksm1,ypeaks0,ypeaksp1,ypeaksp2,ypeaksp2)))

colors=c("red","green","black","black","darkgreen","blue","blueviolet")

par(mar=c(5,5,1,2))
plot(x=U,ypeaksm3,pch=4,cex=0.6,bty="l",col=colors[1],ylim=ylim,ylab="Î",xlab="U / V")
points(x=U,ypeaksm2,pch=4,cex=0.6,bty="l",col=colors[2])
points(x=U,ypeaksm1,pch=4,cex=0.6,bty="l",col=colors[3])
points(x=U,ypeaks0,pch=4,cex=0.6,bty="l",col=colors[4])
points(x=U,ypeaksp1,pch=4,cex=0.6,bty="l",col=colors[5])
points(x=U,ypeaksp2,pch=4,cex=0.6,bty="l",col=colors[6])
points(x=U,ypeaksp3,pch=4,cex=0.6,bty="l",col=colors[7])
grid()

alpha=c()
salpha=c()

m=-3
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksm3),-3)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=-2
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksm2),-2)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=-1
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksm1),-1)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=0
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaks0),0)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=1
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksp1),1)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=2
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksp2),2)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=3
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksp3),3)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

