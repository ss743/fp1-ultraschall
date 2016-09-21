###########################
# Einbinden der Libraries #
###########################
source("readFiles.R")
source("gausfit.R")

######################
# Einlesen der Daten #
######################
data = readCSV("Referenzgitter2.0")
ref=array(dim=c(length(data$Time),2))
ref[,1]=data$Time
ref[,2]=data$CH.A

amp=array(dim=c(2000,2,5))
amp2=array(dim=c(2000,2,5))
suffixes=c("","","_0","_0","_0")

for(i in 1:5){
  suffix=suffixes[i]
  data=readCSV(paste("Gitter",i,suffix,sep=""))
  if(i==1)
    suffix="_0"
  data2=readCSV(paste("Gitter",i,suffix,sep=""))
  time=data$Time
  y=data$CH.A
  time2=data2$Time
  y2=data2$CH.A
  
  amp[,1,i]=time
  amp[,2,i]=y
  amp2[,1,i]=time2
  amp2[,2,i]=y2
}
####################
# Farbdefinitionen #
####################
#colors=c("green","blue","red","yellow","deeppink")

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
sin=c(-1.582,-0.791,0,0.791,1.582)*10^(-4)
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

factor=sum(aval/aerr^2)/sum(1/aerr^2)
errfac=sqrt(1/sum(1/aerr^2))

cat(paste("\nUmrechnungsfaktor a=",factor,"+-",errfac," 1/s\n\n",sep=""))

############################
# Festlegen der Fitgrenzen #
############################

fitgrenzen1=array(dim=c(2,11))
fitgrenzen1[,1]=c(1.75,2.1)
fitgrenzen1[,2]=c(2.3,3.0)
fitgrenzen1[,3]=c(3.0,3.9)
fitgrenzen1[,4]=c(3.9,4.6)
fitgrenzen1[,5]=c(4.7,5.3)
fitgrenzen1[,6]=c(5.3,6.2)
fitgrenzen1[,7]=c(6.2,6.9)
fitgrenzen1[,8]=c(6.9,7.7)
fitgrenzen1[,9]=c(7.7,8.5)
fitgrenzen1[,10]=c(8.5,9.2)
fitgrenzen1[,11]=c(9.3,10.0)
fitgrenzen1=fitgrenzen1*10^(-4)
fitgrenzen1b=array(dim=c(2,1))
fitgrenzen1b[,1]=c(5.3,6.2)
fitgrenzen1b=fitgrenzen1b*10^(-4)

fitgrenzen2=array(dim=c(2,4))
fitgrenzen2[,1]=c(8.5,9.4)
fitgrenzen2[,2]=c(11.3,12.3)
fitgrenzen2[,3]=c(14.2,15.2)
fitgrenzen2[,4]=c(17.2,18.2)
fitgrenzen2=fitgrenzen2*10^(-4)

fitgrenzen3=array(dim=c(2,5))
fitgrenzen3[,1]=c(3.6,4.2)
fitgrenzen3[,2]=c(4.6,5.2)
fitgrenzen3[,3]=c(5.4,6.2)
fitgrenzen3[,4]=c(6.5,7.2)
fitgrenzen3[,5]=c(7.5,8.1)
fitgrenzen3=fitgrenzen3*10^(-4)

fitgrenzen4=array(dim=c(2,5))
fitgrenzen4[,1]=c(2.7,3.25)
fitgrenzen4[,2]=c(4.6,5.2)
fitgrenzen4[,3]=c(5.4,6.2)
fitgrenzen4[,4]=c(6.5,7.2)
fitgrenzen4[,5]=c(8.5,9.1)
fitgrenzen4=fitgrenzen4*10^(-4)

fitgrenzen5=array(dim=c(2,5))
fitgrenzen5[,1]=c(1.7,2.25)
fitgrenzen5[,2]=c(3.5,4.3)
fitgrenzen5[,3]=c(5.4,6.2)
fitgrenzen5[,4]=c(7.4,8.1)
fitgrenzen5[,5]=c(9.5,10)
fitgrenzen5=fitgrenzen5*10^(-4)


#####################
# Plotten der Daten #
#####################
sinamp=amp
sinamp[,1,]=sinamp[,1,]*factor
sinamp2=amp2
sinamp2[,1,]=sinamp2[,1,]*factor

for(i in 1:5){
  plot(sinamp[,1,i],sinamp[,2,i],type="p",pch=4,cex=0.6,bty="l",xlab="sin(theta)",ylab="Amplitude / V")#,col=colors[i])
  grid()
  if(i==1){
    for(j in 1:length(fitgrenzen1[1,])) {
      try({fit=gausfit(data.frame(x=sinamp[,1,i],y=sinamp[,2,i]),bereich=fitgrenzen1[,j])})
      plotgaus(fit, fitgrenzen1[,j])
      printfitdata(fit,paste("Peak ",j,sep=""))
    }
    plot(sinamp2[,1,i],sinamp2[,2,i],type="p",pch=4,cex=0.6,bty="l",xlab="sin(theta)",ylab="Amplitude / V")#,col=colors[i])
    grid()
    for(j in 1:length(fitgrenzen1b[1,])) {
      try({fit=gausfit(data.frame(x=sinamp2[,1,i],y=sinamp2[,2,i]),bereich=fitgrenzen1b[,j])})
      plotgaus(fit, fitgrenzen1b[,j])
      printfitdata(fit,paste("Peak ",j,sep=""))
    }
    
  }
  if(i==2){
    for(j in 1:length(fitgrenzen2[1,])) {
      try({fit=gausfit(data.frame(x=sinamp[,1,i],y=sinamp[,2,i]),bereich=fitgrenzen2[,j])})
      plotgaus(fit, fitgrenzen2[,j])
      printfitdata(fit,paste("Peak ",j,sep=""))
    }
  }
  if(i==3){
    for(j in 1:length(fitgrenzen3[1,])) {
      try({fit=gausfit(data.frame(x=sinamp[,1,i],y=sinamp[,2,i]),bereich=fitgrenzen3[,j])})
      plotgaus(fit, fitgrenzen3[,j])
      printfitdata(fit,paste("Peak ",j,sep=""))
    }
  }
  if(i==4){
    for(j in 1:length(fitgrenzen4[1,])) {
      try({fit=gausfit(data.frame(x=sinamp[,1,i],y=sinamp[,2,i]),bereich=fitgrenzen4[,j])})
      plotgaus(fit, fitgrenzen4[,j])
      printfitdata(fit,paste("Peak ",j,sep=""))
    }
  }
  if(i==5){
    for(j in 1:length(fitgrenzen5[1,])) {
      try({fit=gausfit(data.frame(x=sinamp[,1,i],y=sinamp[,2,i]),bereich=fitgrenzen5[,j])})
      plotgaus(fit, fitgrenzen5[,j])
      printfitdata(fit,paste("Peak ",j,sep=""))
    }
  }
  
}
