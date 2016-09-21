###########################
# Einbinden der Libraries #
###########################
source("readFiles.R")
source("gausfit.R")

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



####################
# Farbdefinitionen #
####################
colors=c("cadetblue1","cadetblue","blue","seagreen","green4","green","chartreuse","gold","orange","darkorange","orangered","red")


#####################
# Plotten der Daten #
#####################
xlim=c(0,0.00025)
ylim=c(0,10)
zlim=c(1,12)
p=scatterplot3d(xlim,zlim,ylim,type="n",box=FALSE,grid=TRUE,y.ticklabs=c(""),xlab=expression(sin(theta)),ylab="",zlab="Amplitude / V")

sinamp=gitter
sinamp[,1,]=sinamp[,1,]*factor
#plot(sinamp[,1,1],sinamp[,2,1],type="n",bty="l")
for(i in 1:N){
  p$points3d(sinamp[,1,i],((13-i)*sinamp[,2,i]/sinamp[,2,i]),sinamp[,2,i],type="p",pch=4,cex=0.5,col=colors[i])
  #p$points3d()
  #grid()

}
