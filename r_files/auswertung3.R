source("besselsquare.R")
library(plotrix)

peaksm3=c(-5.28,-5.40,-5.47,-5.37,-5.39,-5.54,-5.70,-5.70,-5.37,-5.23,-5.46,-5.28)*10^(-5)*corrfac
peaksm2=c(-3.45,-3.57,-3.64,-3.54,-3.56,-3.71,-3.87,-3.87,-3.54,-3.54,-3.65,-3.42)*10^(-5)*corrfac
peaksm1=c(-1.67,-1.79,-1.86,-1.76,-1.78,-1.93,-2.09,-2.09,-1.75,-1.65,-1.86,-1.65)*10^(-5)*corrfac
peaks0=c(1.23,-0.09,-0.68,0.257,0.1,-1.35,-2.96,-2.95,0.3,1.66,-0.6,1.2)*10^(-6)*corrfac
peaksp1=c(1.97,1.85,1.78,1.88,1.86,1.71,1.55,1.55,1.87,2.06,1.83,1.90)*10^(-5)*corrfac
peaksp2=c(3.86,3.74,3.69,3.77,3.75,3.60,3.44,3.44,3.77,3.98,3.78,3.70)*10^(-5)*corrfac
peaksp3=c(5.59,5.47,5.40,5.50,5.48,5.33,5.17,5.17,5.50,5.64,5.41,5.59)*10^(-5)*corrfac

speaksm3=c(0.09,0.09,0.09,0.09,0.09,0.09,0.09,0.09,0.09,0.09,0.09,0.09)*10^(-5)*corrfac
speaksm2=c(0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.05,0.17,0.06,0.06)*10^(-5)*corrfac
speaksm1=c(0.02,0.02,0.03,0.02,0.03,0.03,0.02,0.02,0.03,0.06,0.04,0.04)*10^(-5)*corrfac
speaks0=c(0.09,0.10,0.15,0.018,0.15,0.16,0.08,0.08,0.3,0.18,0.2,0.3)*10^(-6)*corrfac
speaksp1=c(0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.09,0.05,0.05)*10^(-5)*corrfac
speaksp2=c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.06,0.13,0.07,0.08)*10^(-5)*corrfac
speaksp3=c(0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13,0.13)*10^(-5)*corrfac

U=c(0.00,0.88,1.73,2.43,3.20,4.20,5.10,6.08,7.04,8.01,8.97,9.69)

peakm3=-5.4*10^(-5)*corrfac
peakm2=-3.57*10^(-5)*corrfac
peakm1=-1.79*10^(-5)*corrfac
peak0=0
peakp1=1.85*10^(-5)*corrfac
peakp2=3.74*10^(-5)*corrfac
peakp3=5.47*10^(-5)*corrfac

ypeaksm3=c()
ypeaksm2=c()
ypeaksm1=c()
ypeaks0=c()
ypeaksp1=c()
ypeaksp2=c()
ypeaksp3=c()
sypeaksm3=c()
sypeaksm2=c()
sypeaksm1=c()
sypeaks0=c()
sypeaksp1=c()
sypeaksp2=c()
sypeaksp3=c()

for(i in 1:12){
  
  ypeaksm3[i]=gausfunction(fits[[i]],peaksm3[i])
  ypeaksm2[i]=gausfunction(fits[[i]],peaksm2[i])
  ypeaksm1[i]=gausfunction(fits[[i]],peaksm1[i])
  ypeaks0[i] =gausfunction(fits[[i]],peaks0[i])
  ypeaksp1[i]=gausfunction(fits[[i]],peaksp1[i])
  ypeaksp2[i]=gausfunction(fits[[i]],peaksp2[i])
  ypeaksp3[i]=gausfunction(fits[[i]],peaksp3[i])
  
  sypeaksm3[i]=(abs(gausfunction(fits[[i]],peaksm3[i])-gausfunction(fits[[i]],peaksm3[i]-speaksm3[i]))+abs(gausfunction(fits[[i]],peaksm3[i])-gausfunction(fits[[i]],peaksm3[i]+speaksm3[i])))/2
  sypeaksm2[i]=(abs(gausfunction(fits[[i]],peaksm2[i])-gausfunction(fits[[i]],peaksm2[i]-speaksm2[i]))+abs(gausfunction(fits[[i]],peaksm3[i])-gausfunction(fits[[i]],peaksm3[i]+speaksm3[i])))/2
  sypeaksm1[i]=(abs(gausfunction(fits[[i]],peaksm1[i])-gausfunction(fits[[i]],peaksm1[i]-speaksm1[i]))+abs(gausfunction(fits[[i]],peaksm3[i])-gausfunction(fits[[i]],peaksm3[i]+speaksm3[i])))/2
  sypeaks0[i] =(abs(gausfunction(fits[[i]],peaks0[i])-gausfunction(fits[[i]],peaks0[i]-speaks0[i]))+abs(gausfunction(fits[[i]],peaksm3[i])-gausfunction(fits[[i]],peaksm3[i]+speaksm3[i])))/2
  sypeaksp1[i]=(abs(gausfunction(fits[[i]],peaksp1[i])-gausfunction(fits[[i]],peaksp1[i]-speaksp1[i]))+abs(gausfunction(fits[[i]],peaksm3[i])-gausfunction(fits[[i]],peaksm3[i]+speaksm3[i])))/2
  sypeaksp2[i]=(abs(gausfunction(fits[[i]],peaksp2[i])-gausfunction(fits[[i]],peaksp2[i]-speaksp2[i]))+abs(gausfunction(fits[[i]],peaksm3[i])-gausfunction(fits[[i]],peaksm3[i]+speaksm3[i])))/2
  sypeaksp3[i]=(abs(gausfunction(fits[[i]],peaksp3[i])-gausfunction(fits[[i]],peaksp3[i]-speaksp3[i]))+abs(gausfunction(fits[[i]],peaksm3[i])-gausfunction(fits[[i]],peaksm3[i]+speaksm3[i])))/2
  
}

speaksm3[12]=

normfactor=ypeaks0[1]

ypeaksm3=ypeaksm3/normfactor
ypeaksm2=ypeaksm2/normfactor
ypeaksm1=ypeaksm1/normfactor
ypeaks0=ypeaks0/normfactor
ypeaksp1=ypeaksp1/normfactor
ypeaksp2=ypeaksp2/normfactor
ypeaksp3=ypeaksp3/normfactor

ylim=c(min(c(ypeaksm3,ypeaksm2,ypeaksm1,ypeaks0,ypeaksp1,ypeaksp2,ypeaksp2)),max(c(ypeaksm3,ypeaksm2,ypeaksm1,ypeaks0,ypeaksp1,ypeaksp2,ypeaksp2)))

colors=c("darkred","green","black","deeppink","blue","blueviolet","darkgreen")

par(mar=c(5,5,1,2))
plotCI(x=U,ypeaksm3,pch=4,cex=0.6,bty="l",col=colors[1],ylim=ylim,ylab="Î",xlab="U / V",uiw=sypeaksm3)
plotCI(x=U,ypeaksm2,pch=4,cex=0.6,bty="l",col=colors[2],uiw=sypeaksm2,add=TRUE)
plotCI(x=U,ypeaksm1,pch=4,cex=0.6,bty="l",col=colors[3],uiw=sypeaksm1,add=TRUE)
plotCI(x=U,ypeaks0,pch=4,cex=0.6,bty="l",col=colors[4],uiw=sypeaks0,add=TRUE)
plotCI(x=U,ypeaksp1,pch=4,cex=0.6,bty="l",col=colors[5],uiw=sypeaksp1,add=TRUE)
plotCI(x=U,ypeaksp2,pch=4,cex=0.6,bty="l",col=colors[6],uiw=sypeaksp2,add=TRUE)
plotCI(x=U,ypeaksp3,pch=4,cex=0.6,bty="l",col=colors[7],uiw=sypeaksp3,add=TRUE)
grid()

alpha=c()
salpha=c()

m=-3
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksm3,sy=sypeaksm3),-3,weighted=TRUE)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=-2
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksm2,sy=sypeaksm2),-2,weighted=TRUE)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=-1
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksm1,sy=sypeaksm1),-1,weighted=TRUE)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=0
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaks0,sy=sypeaks0),0,weighted=TRUE)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=1
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksp1,sy=sypeaksp1),1,weighted=TRUE)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=2
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksp2,sy=sypeaksp2),2,weighted=TRUE)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=3
try({
  i=m+4
  fitdata=besselsquarefit(data.frame(x=U,y=ypeaksp3,sy=sypeaksp3),3,weighted=FALSE)
  alpha[i]=fitdata["alpha","Estimate"]
  salpha[i]=fitdata["alpha","Std. Error"]
  plot(function(x){(besselJ(x*alpha[i],m))^2},0,10,col=colors[i],add=TRUE)
})

m=c(-3,-2,-1,0,1,2,3)
legendvector=c()
for(i in 1:length(m)){
  legendvector[i]=paste("m = ",m[i],sep="")
}
legend(x=7,y=0.9,legendvector,fill=colors,cex=0.7,bty="n")

