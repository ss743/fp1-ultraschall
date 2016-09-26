###########################
# Einbinden der Libraries #
###########################
source("readFiles.R")
source("gausfit.R")
source("round.R")
library("scatterplot3d")

######################
# Einlesen der Daten #
######################
data = readCSV("Eichung_ultraschall")
ref=array(dim=c(length(data$Time),2))
ref[,1]=data$Time
ref[,2]=data$CH.A

Ustring=c("0-00","0-88","1-73","2-43","3-20","4-20","5-10","6-08","7-04","8-01","8-97","9-69")

N=length(Ustring)
gitter=array(dim=c(2000,2,N))


for(i in 1:N){
  suffix="V"
  data=readCSV(paste("ultraschall_",Ustring[i],suffix,sep=""))
  #if(i==1)
  #  suffix="_0"
  #data2=readCSV(paste("Gitter",i,suffix,sep=""))
  time=data$Time
  y=data$CH.A
  #time2=data2$Time
  #y2=data2$CH.A
  
  gitter[,1,i]=time
  gitter[,2,i]=y
  #amp2[,1,i]=time2
  #amp2[,2,i]=y2
}

###############
# Zeiteichung #
###############
plot(ref[,1],ref[,2],type="p",pch=4,cex=0.6,bty="l",xlab="t / s",ylab="Amplitude / V")#,col=colors[i])
grid()
fitgrenzen=array(dim=c(2,5))
fitgrenzen[,1]=c(0.9,1.3)
fitgrenzen[,2]=c(1.4,2.1)
fitgrenzen[,3]=c(2.2,2.8)
fitgrenzen[,4]=c(2.9,3.5)
fitgrenzen[,5]=c(3.5,4.1)
fitgrenzen=fitgrenzen*10^(-4)
mu=array(dim=c(5,2))
t=array(dim=c(5,2))
a=array(dim=c(5,2))
#sin=c(-1.582,-0.791,0,0.791,1.582)*10^(-4)
sin=c(-0.0101248,-0.0050624,0,0.0050624,0.0101248)
for(i in 1:5){
  fit=gausfit(data.frame(x=ref[,1],y=ref[,2]),bereich=fitgrenzen[,i])
  plotgaus(fit, fitgrenzen[,i])
  printfitdata(fit,paste("Peak ",i,sep=""))
  mu[i,]=getmu(fit)
}
for(i in c(1,2,4,5)){
  t[i,1]=mu[i,1]-mu[3,1]
  t[i,2]=sqrt(mu[3,2]^2+mu[i,2]^2)
  cat(paste("\nt_",i," = ",t[i,1],"+-",t[i,2]," s",sep=""))
  a[i,1]=sin[i]/t[i,1]
  a[i,2]=t[i,2]/t[i,1]*a[i,1]
}
aval=a[,1]
aval=aval[!is.na(aval)]
aerr=a[,2]
aerr=aerr[!is.na(aerr)]

factor=70.64#sum(aval/aerr^2)/sum(1/aerr^2)
errfac=0.06#sqrt(1/sum(1/aerr^2))

corrfac=factor/1.117575


cat(paste("\nUmrechnungsfaktor a=",factor,"+-",errfac," 1/s\n\n",sep=""))

#######################
# Zeit-Ort-Umrechnung #
#######################
sinamp=gitter
sinamp[,1,]=sinamp[,1,]*factor


######################################################
# Gaußfits an Nulltes Maximum zur Positionskorrektur #
######################################################
grenzen=c(10,12)*10^(-5)*corrfac
mittelpunkte=c()

for(i in 1:12){
  
  try({
    mittelpunkt=gausfit(data.frame(x=sinamp[,1,i],y=sinamp[,2,i]),grenzen)
    #plotgaus(mittelpunkt,grenzen)
    mittelpunkte[i]=mittelpunkt["mu","Estimate"]
  })
  #print(i)
}

sinamp[,1,]=sinamp[,1,]-mittelpunkte



############################
# Festlegen der Fitgrenzen #
############################
fitgrenzen=array(dim=c(9,2))
fitgrenzen[1,]=c(-1,1)
fitgrenzen[2,]=c(-2.7,-1)
fitgrenzen[3,]=c(1,2.6)
fitgrenzen[4,]=c(-4.5,-2.7)
fitgrenzen[5,]=c(3,5)
fitgrenzen[6,]=c(-6.5,-4.5)
fitgrenzen[7,]=c(5,6.5)
fitgrenzen[8,]=c(-8.1,-6.5)
fitgrenzen[9,]=c(7,8.5)

fitgrenzen=fitgrenzen*10^(-5)*corrfac


####################
# Farbdefinitionen #
####################
colors=c("cadetblue1","cadetblue","blue","seagreen","green4","green","chartreuse","gold","orange","darkorange","orangered","red")


#####################
# Plotten der Daten #
#####################
xlim=c(-0.000125,0.000125)*corrfac
ylim=c(0,10)
zlim=c(1,12)
p=scatterplot3d(xlim,zlim,ylim,type="n",box=FALSE,grid=TRUE,y.ticklabs=c(""),xlab=expression(sin(theta)),ylab="",zlab="I / V",mar=c(4,4,1,1))

for(i in 1:N){
  p$points3d(sinamp[,1,i],((13-i)*sinamp[,2,i]/sinamp[,2,i]),sinamp[,2,i],type="p",pch=4,cex=0.5,col=colors[i])
  #k=i
  #try({
  #  fitdata=ninegausfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),sig0=5.5*10^(-6))
  #  #plotninegaus(fitdata,c(min(sinamp[,1,k]),max(sinamp[,1,k])),p,col="black",z=13-i)
  #  printninefitdata(fitdata)
  #  mus=getmus(fitdata)
  #})
  
}

U=c(0.00,0.88,1.73,2.43,3.20,4.20,5.10,6.08,7.04,8.01,8.97,9.69)
legendvector=c()
for(i in 1:12){
  legendvector[i]=paste("U = ",U[i]," V",sep="")
}

legend(x=-1.5,y=7.6,legendvector,fill=colors,cex=0.7,bty="n")

#k=12
#plot(sinamp[,1,k],sinamp[,2,k],bty="l",pch=4,cex=0.6)
#grid()
#fitdata=gausfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),fitgrenzen[1,])
#plotgaus(fitdata,fitgrenzen[1,])
#printfitdata(fitdata)
#for(i in c(2,3,4,5)){
#  try({ 
#    if(i==3)
#      fitdata=gauslinfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),fitgrenzen[i,],B0=-0.2/1.6*10^5)
#    else
#      fitdata=gauslinfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),fitgrenzen[i,])
#  plotgauslin(fitdata,fitgrenzen[i,])
#  plotgaus(fitdata,c(-1,1)*10^(-4),C=0,col="blue")
#  printfitdata(fitdata)
#  mu=fitdata["mu","Estimate"]
#  abline(v=mu)})
#}

#k=12
a=function(x){x}
fits=c()

for(k in 1:6){
  par(mar=c(5,5,2,2))
  plot(sinamp[,1,k],sinamp[,2,k],bty="l",pch=4,cex=0.6,xlab=expression(sin(theta)),ylab="I / V")
  grid()
  title(legendvector[k])
  try({
    fitdata=threegausfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),sig0=5.5*10^(-6)*corrfac)
    fits[[k]]=fitdata
    plotthreegaus(fitdata,c(min(sinamp[,1,k]),max(sinamp[,1,k])))
    mus=getmus3(fitdata)
    smus=getmuerrs3(fitdata)
    for(i in 2:2){
      abline(v=mus[i])
      rmu=roundfunc(c(mus[i],smus[i]))
      text(x=5*10^(-3),y=(5-0.2*i),paste("Peak ",i-1," bei sin(theta)=",rmu[1],"+-",rmu[2],sep=""),cex=0.6)
    }
    printthreefitdata(fitdata)
    
  })
}
for(k in 7:7){
  par(mar=c(5,5,2,2))
  plot(sinamp[,1,k],sinamp[,2,k],bty="l",pch=4,cex=0.6,xlab=expression(sin(theta)),ylab="I / V")
  grid()
  title(legendvector[k])
  try({
    fitdata=threegausfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),sig0=5*10^(-6)*corrfac)
    fits[[k]]=fitdata
    plotthreegaus(fitdata,c(min(sinamp[,1,k]),max(sinamp[,1,k])))
    mus=getmus3(fitdata)
    smus=getmuerrs3(fitdata)
    for(i in 2:2){
      abline(v=mus[i])
      rmu=roundfunc(c(mus[i],smus[i]))
      text(x=5*10^(-3),y=(5-0.2*i),paste("Peak ",i-1," bei sin(theta)=",rmu[1],"+-",rmu[2],sep=""),cex=0.6)
    }
    printthreefitdata(fitdata)
    
  })
}

for(k in 8:8){
  par(mar=c(5,5,2,2))
  plot(sinamp[,1,k],sinamp[,2,k],bty="l",pch=4,cex=0.6,xlab=expression(sin(theta)),ylab="I / V")
  grid()
  title(legendvector[k])
  try({
    fitdata=threegausfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),sig0=5.5*10^(-6)*corrfac)
    fits[[k]]=fitdata
    plotthreegaus(fitdata,c(min(sinamp[,1,k]),max(sinamp[,1,k])))
    mus=getmus3(fitdata)
    smus=getmuerrs3(fitdata)
    for(i in 2:2){
      abline(v=mus[i])
      rmu=roundfunc(c(mus[i],smus[i]))
      text(x=5*10^(-3),y=(5-0.2*i),paste("Peak ",i-1," bei sin(theta)=",rmu[1],"+-",rmu[2],sep=""),cex=0.6)
    }
    printthreefitdata(fitdata)
    
  })
}
for(k in 9:9){
  par(mar=c(5,5,2,2))
  plot(sinamp[,1,k],sinamp[,2,k],bty="l",pch=4,cex=0.6,xlab=expression(sin(theta)),ylab="I / V")
  grid()
  title(legendvector[k])
  try({
    fitdata=fivegausfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),sig0=5.5*10^(-6)*corrfac)
    fits[[k]]=fitdata
    plotfivegaus(fitdata,c(min(sinamp[,1,k]),max(sinamp[,1,k])))
    mus=getmus5(fitdata)
    smus=getmuerrs5(fitdata)
    for(i in 2:4){
      abline(v=mus[i])
      rmu=roundfunc(c(mus[i],smus[i]))
      text(x=5*10^(-3),y=(5-0.2*i),paste("Peak ",i-1," bei sin(theta)=",rmu[1],"+-",rmu[2],sep=""),cex=0.6)
    }
    printfivefitdata(fitdata)
    
  })
}

for(k in 10:11){
  par(mar=c(5,5,2,2))
  plot(sinamp[,1,k],sinamp[,2,k],bty="l",pch=4,cex=0.6,xlab=expression(sin(theta)),ylab="I / V")
  grid()
  title(legendvector[k])
  try({
    fitdata=sevengausfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),sig0=5.5*10^(-6)*corrfac)
    fits[[k]]=fitdata
    plotsevengaus(fitdata,c(min(sinamp[,1,k]),max(sinamp[,1,k])))
    mus=getmus5(fitdata)
    smus=getmuerrs5(fitdata)
    for(i in 1:5){
      abline(v=mus[i])
      rmu=roundfunc(c(mus[i],smus[i]))
      text(x=5*10^(-3),y=(3.2-0.2*i),paste("Peak ",i," bei sin(theta)=",rmu[1],"+-",rmu[2],sep=""),cex=0.6)
    }
    printsevenfitdata(fitdata)
    
  })
}
for(k in 12:12){
  par(mar=c(5,5,2,2))
  plot(sinamp[,1,k],sinamp[,2,k],bty="l",pch=4,cex=0.6,xlab=expression(sin(theta)),ylab="I / V")
  grid()
  title(legendvector[k])
  try({
    fitdata=ninegausfit(data.frame(x=sinamp[,1,k],y=sinamp[,2,k]),sig0=5.5*10^(-6)*corrfac)
    fits[[k]]=fitdata
    plotninegaus(fitdata,c(min(sinamp[,1,k]),max(sinamp[,1,k])))
    printninefitdata(fitdata)
    mus=getmus(fitdata)
    smus=getmuerrs(fitdata)
    for(i in 1:7){
      abline(v=mus[i])
      rmu=roundfunc(c(mus[i],smus[i]))
      text(x=6*10^(-3),y=(2.5-0.2*i),paste("Peak ",i," bei sin(theta)=",rmu[1],"+-",rmu[2],sep=""),cex=0.6)
    }

  })
}

source("auswertung3.R")