gausfit <- function(x,y,sy=1,bereich=c(input$x[1],input$x[length(input$x)]),weighted=FALSE,sig0=0,N0=0){
  gausfit(data.frame(x=x,y=y,sy=sy),bereich,weighted,sig0,N0)
}


gausfit <- function(input,bereich=c(input$x[1],input$x[length(input$x)]),weighted=FALSE,sig0=0,N0=0){ #--- Fitten der Exponentialfunktion
  
  #print(bereich)
  thegaussian <- y ~ C + N*1/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))
  
  daten=subset(input,x>=bereich[1] & x<= bereich[2])
  #print(daten)
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
  } else {
    ymax=N0
  }
  mu0 =daten$x[which.max(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }
  N0=(sqrt(2*pi)*sig0)*(ymax-ymin)
  #cat(paste("\nC=",ymin,"\nN=",N0,"\nmu=",mu0,"\nsigma=",sig0,sep=""))
  #plot (function(x){ymin + N0/(sqrt(2*pi)*sig0)*exp(-(x-mu0)^2/(2*sig0^2))},bereich[1],bereich[2],add=TRUE,col="green")
  if(weighted)
  {
    err=daten$sy
    fit = nls(thegaussian,daten,weights=1/err^2,start=list(C=ymin,N=N0,mu=mu0,sig=sig0))
  }
  else
    fit = nls(thegaussian,daten,start=list(C=ymin,N=ymax,mu=mu0,sig=sig0))
  
  return(summary(fit)$parameters)
  
}

threegausfit <- function(input,bereich=c(input$x[1],input$x[length(input$x)]),weighted=FALSE,sig0=0,N0=0){ #--- Fitten der Exponentialfunktion
  
  #print(bereich)
  thegaussian <- y ~ C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))
  
  daten=subset(input,x>=bereich[1] & x<= bereich[2])
  #print(daten)
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
    N0=(sqrt(2*pi)*sig0)*(ymax-ymin)
  } else {
    N0=N0
  }
  mu0 =daten$x[which.max(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }
  N0=c(0.5,0.5,0.5)
  #cat(paste("\nC=",ymin,"\nN=",N0,"\nmu=",mu0,"\nsigma=",sig0,sep=""))
  #plot (function(x){ymin + N0/(sqrt(2*pi)*sig0)*exp(-(x-mu0)^2/(2*sig0^2))},bereich[1],bereich[2],add=TRUE,col="green")
  #plot(function(x){C + N0[1]*1/(sqrt(2*pi)*sig0)*exp(-(x+1.72*10^(-5))^2/(2*sig0^2))+  N0[2]*1/(sqrt(2*pi)*sig0)*exp(-(x-0.19*10^(-5))^2/(2*sig0^2))+  N0[3]*1/(sqrt(2*pi)*sig0)*exp(-(x-2.4*10^(-5))^2/(2*sig0^2))},bereich[1],bereich[2],add=TRUE,col="green")
  if(weighted)
  {
    err=daten$sy
    fit = nls(thegaussian,daten,weights=1/err^2,start=list(C=ymin,N=N0,mu=mu0,sig=sig0))
  }
  else
    fit = nls(thegaussian,daten,start=list(C=ymin,N1=N0[1],N2=N0[2],N3=N0[3],mu1=-1.72*10^(-5)*corrfac,mu2=0.19*10^(-5)*corrfac,mu3=2.4*10^(-5)*corrfac,sig1=sig0,sig2=sig0,sig3=sig0))
  
  return(summary(fit)$parameters)
  
}


fivegausfit <- function(input,bereich=c(input$x[1],input$x[length(input$x)]),weighted=FALSE,sig0=0,N0=0){ #--- Fitten der Exponentialfunktion
  
  #print(bereich)
  thegaussian <- y ~ C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2))
  
  daten=subset(input,x>=bereich[1] & x<= bereich[2])
  #print(daten)
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
    N0=(sqrt(2*pi)*sig0)*(ymax-ymin)
  } else {
    N0=N0
  }
  mu0 =daten$x[which.max(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }
  N0=c(0.5,0.5,0.5,0.5,0.5)
  #cat(paste("\nC=",ymin,"\nN=",N0,"\nmu=",mu0,"\nsigma=",sig0,sep=""))
  #plot (function(x){ymin + N0/(sqrt(2*pi)*sig0)*exp(-(x-mu0)^2/(2*sig0^2))},bereich[1],bereich[2],add=TRUE,col="green")
  if(weighted)
  {
    err=daten$sy
    fit = nls(thegaussian,daten,weights=1/err^2,start=list(C=ymin,N=N0,mu=mu0,sig=sig0))
  }
  else
    fit = nls(thegaussian,daten,start=list(C=ymin,N1=N0[1],N2=N0[2],N3=N0[3],N4=N0[4],N5=N0[5],mu1=-3.52*10^(-5)*corrfac,mu2=-1.72*10^(-5)*corrfac,mu3=0.19*10^(-5)*corrfac,mu4=2.4*10^(-5)*corrfac,mu5=3.78*10^(-5)*corrfac,sig1=sig0,sig2=sig0,sig3=sig0,sig4=sig0,sig5=sig0))
  
  return(summary(fit)$parameters)
  
}

ninegausfit <- function(input,bereich=c(input$x[1],input$x[length(input$x)]),weighted=FALSE,sig0=0,N0=0){ #--- Fitten der Exponentialfunktion
  
  #print(bereich)
  thegaussian <- y ~ C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2)) + N8*1/(sqrt(2*pi)*sig8)*exp(-(x-mu8)^2/(2*sig8^2)) + N9*1/(sqrt(2*pi)*sig9)*exp(-(x-mu9)^2/(2*sig9^2))
  
  daten=subset(input,x>=bereich[1] & x<= bereich[2])
  #print(daten)
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
    N0=(sqrt(2*pi)*sig0)*(ymax-ymin)
  } else {
    N0=N0
  }
  mu0 =daten$x[which.max(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }
  N0=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
  #cat(paste("\nC=",ymin,"\nN=",N0,"\nmu=",mu0,"\nsigma=",sig0,sep=""))
  #plot (function(x){ymin + N0/(sqrt(2*pi)*sig0)*exp(-(x-mu0)^2/(2*sig0^2))},bereich[1],bereich[2],add=TRUE,col="green")
  if(weighted)
  {
    err=daten$sy
    fit = nls(thegaussian,daten,weights=1/err^2,start=list(C=ymin,N=N0,mu=mu0,sig=sig0))
  }
  else
    fit = nls(thegaussian,daten,start=list(C=ymin,N1=N0[1],N2=N0[2],N3=N0[3],N4=N0[4],N5=N0[5],N6=N0[6],N7=N0[7],N8=N0[8],N9=N0[9],mu1=-3.52*10^(-5)*corrfac,mu2=-1.72*10^(-5)*corrfac,mu3=0.19*10^(-5)*corrfac,mu4=2.4*10^(-5)*corrfac,mu5=3.78*10^(-5)*corrfac,mu6=-5.5*10^(-5)*corrfac,mu7=6*10^(-5)*corrfac,mu8=-7.3*10^(-5)*corrfac,mu9=8*10^(-5)*corrfac,sig1=sig0,sig2=sig0,sig3=sig0,sig4=sig0,sig5=sig0,sig6=sig0,sig7=sig0,sig8=sig0,sig9=sig0))
  
  return(summary(fit)$parameters)
  
}

sevengausfit <- function(input,bereich=c(input$x[1],input$x[length(input$x)]),weighted=FALSE,sig0=0,N0=0){ #--- Fitten der Exponentialfunktion
  
  #print(bereich)
  thegaussian <- y ~ C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2))
  
  daten=subset(input,x>=bereich[1] & x<= bereich[2])
  #print(daten)
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
    N0=(sqrt(2*pi)*sig0)*(ymax-ymin)
  } else {
    N0=N0
  }
  mu0 =daten$x[which.max(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }
  N0=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5)
  #cat(paste("\nC=",ymin,"\nN=",N0,"\nmu=",mu0,"\nsigma=",sig0,sep=""))
  #plot (function(x){ymin + N0/(sqrt(2*pi)*sig0)*exp(-(x-mu0)^2/(2*sig0^2))},bereich[1],bereich[2],add=TRUE,col="green")
  if(weighted)
  {
    err=daten$sy
    fit = nls(thegaussian,daten,weights=1/err^2,start=list(C=ymin,N=N0,mu=mu0,sig=sig0))
  }
  else
    fit = nls(thegaussian,daten,start=list(C=ymin,N1=N0[1],N2=N0[2],N3=N0[3],N4=N0[4],N5=N0[5],N6=N0[6],N7=N0[7],mu1=-3.52*10^(-5)*corrfac,mu2=-1.72*10^(-5)*corrfac,mu3=0.19*10^(-5)*corrfac,mu4=2.4*10^(-5)*corrfac,mu5=3.78*10^(-5)*corrfac,mu6=-5.5*10^(-5)*corrfac,mu7=6*10^(-5)*corrfac,sig1=sig0,sig2=sig0,sig3=sig0,sig4=sig0,sig5=sig0,sig6=sig0,sig7=sig0))
  
  return(summary(fit)$parameters)
  
}



gauslinfit <- function(input,bereich=c(input$x[1],input$x[length(input$x)]),weighted=FALSE,sig0=0,N0=0,B0=NA){ #--- Fitten der Exponentialfunktion
  #print("Bumm")
  #print(bereich)
  daten=subset(input,x>=bereich[1] & x<= bereich[2])
  
  N=length(daten$x)
  
  if(is.na(B0))
    B0=(daten$y[N]-daten$y[1])/(daten$x[N]-daten$x[1])
  
  #print(daten)
  ymin=min(daten$y)-B0*daten$x[which.min(daten$y)]
  if(N0==0){
    ymax=max(daten$y)-B0*daten$x[which.max(daten$y)]
  } else {
    ymax=N0
  }
  mu0 =daten$x[which.max(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }
  N0=(sqrt(2*pi)*sig0)*(ymax-ymin)
  #cat(paste("\nC=",ymin,"\nN=",N0,"\nmu=",mu0,"\nsigma=",sig0,"\nB=",B0,sep=""))
  
  plot (function(x){ymin +B0*x+ N0/(sqrt(2*pi)*sig0)*exp(-(x-mu0)^2/(2*sig0^2))},bereich[1],bereich[2],add=TRUE,col="green")
  
  thegaussian <- y ~ C+B*x + N*1/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))
  
  if(weighted)
  {
    err=daten$sy
    fit = nls(thegaussian,daten,weights=1/err^2,start=list(C=ymin,N=N0,mu=mu0,sig=sig0,B=B0))
  }
  else
    fit = nls(thegaussian,daten,start=list(C=ymin,N=N0,mu=mu0,sig=sig0,B=B0))
  
  return(summary(fit)$parameters)
  
}


#gausfit <- function(input,weighted=FALSE,sig0=0,N0=0){
# 
#  bereich=
#  gausfit(input,bereich,weighted,sig0,N0)
#}

#gausfit <- function(x,y,weighted=FALSE,sig0=0,N0=0,sy=0){
  
#  bereich=c(x[1],x[length(x)])
#  gausfit(x,y,bereich,weighted,sig0,N0,sy)
#}


plotgaus <- function(fitdata,bereich,C=NA,col="red"){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N<-fitdata["N","Estimate"]
  if(is.na(C))
    C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  sig<-fitdata["sig","Estimate"]
  
  plot (function(x){C + N*1/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))},bereich[1],bereich[2],add=TRUE,col=col)
  
  return(function(x){C + N*1/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))})
}


plotthreegaus <- function(fitdata,bereich,col="red"){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  C<-fitdata["C","Estimate"]
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]

  plot (function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))},bereich[1],bereich[2],add=TRUE,col=col)
  
  return(function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))})
}


plotfivegaus <- function(fitdata,bereich,col="red"){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  N4<-fitdata["N4","Estimate"]
  N5<-fitdata["N5","Estimate"]
  C<-fitdata["C","Estimate"]
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  sig4<-fitdata["sig4","Estimate"]
  sig5<-fitdata["sig5","Estimate"]
  
  plot (function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2))},bereich[1],bereich[2],add=TRUE,col=col)
  
  return(function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2))})
}

plotsevengaus <- function(fitdata,bereich,col="red"){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  N4<-fitdata["N4","Estimate"]
  N5<-fitdata["N5","Estimate"]
  N6<-fitdata["N6","Estimate"]
  N7<-fitdata["N7","Estimate"]
  C<-fitdata["C","Estimate"]
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  mu6<-fitdata["mu6","Estimate"]
  mu7<-fitdata["mu7","Estimate"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  sig4<-fitdata["sig4","Estimate"]
  sig5<-fitdata["sig5","Estimate"]
  sig6<-fitdata["sig6","Estimate"]
  sig7<-fitdata["sig7","Estimate"]

  plot (function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2))},bereich[1],bereich[2],add=TRUE,col=col)
  
  return(function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2))})
}

plotninegaus <- function(fitdata,bereich,col="red"){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  N4<-fitdata["N4","Estimate"]
  N5<-fitdata["N5","Estimate"]
  N6<-fitdata["N6","Estimate"]
  N7<-fitdata["N7","Estimate"]
  N8<-fitdata["N8","Estimate"]
  N9<-fitdata["N9","Estimate"]
  C<-fitdata["C","Estimate"]
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  mu6<-fitdata["mu6","Estimate"]
  mu7<-fitdata["mu7","Estimate"]
  mu8<-fitdata["mu8","Estimate"]
  mu9<-fitdata["mu9","Estimate"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  sig4<-fitdata["sig4","Estimate"]
  sig5<-fitdata["sig5","Estimate"]
  sig6<-fitdata["sig6","Estimate"]
  sig7<-fitdata["sig7","Estimate"]
  sig8<-fitdata["sig8","Estimate"]
  sig9<-fitdata["sig9","Estimate"]
  
  plot (function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2)) + N8*1/(sqrt(2*pi)*sig8)*exp(-(x-mu8)^2/(2*sig8^2)) + N9*1/(sqrt(2*pi)*sig9)*exp(-(x-mu9)^2/(2*sig9^2))},bereich[1],bereich[2],add=TRUE,col=col)
  
  return(function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2)) + N8*1/(sqrt(2*pi)*sig8)*exp(-(x-mu8)^2/(2*sig8^2)) + N9*1/(sqrt(2*pi)*sig9)*exp(-(x-mu9)^2/(2*sig9^2))})
}




plotgauslin <- function(fitdata,bereich){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N<-fitdata["N","Estimate"]
  C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  sig<-fitdata["sig","Estimate"]
  B<-fitdata["B","Estimate"]
  
  plot (function(x){C+ B*x+ N*1/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))},bereich[1],bereich[2],add=TRUE,col="red")
  
}


texfitdata <- function(fitdata,title=""){ #--- Ausgabe der Gaußfit-Daten
  
  #mu=c()
  #smu=c()
  #A=c()
  #sA=c()
  #sig=c()
  #ssig=c()
  
  #for(i in 1:9){
  #  try({
  #    mu[i]<-fitdata["mu","Estimate"]
  #    smu[i]<-fitdata["mu","Std. Error"]
  #    A[i]<-fitdata["N","Estimate"]
  #    sA[i]<-fitdata["N","Std. Error"]
  #    sig[i]<-fitdata["sig","Estimate"]
  #    ssig[i]<-fitdata["sig","Std. Error"]
   #   
  #    N=i
  #  })
  #}
  #cat(paste(title,"\\\\\n",sep=""))

  source("round.R")
  
  mu<-fitdata["mu","Estimate"]
  smu<-fitdata["mu","Std. Error"]
  A<-fitdata["N","Estimate"]
  sA<-fitdata["N","Std. Error"]
  sig<-fitdata["sig","Estimate"]
  ssig<-fitdata["sig","Std. Error"]
  
  mus<-roundfunc(c(mu,smu))
  mu<-mus[1]
  smu<-mus[2]
  As<-roundfunc(c(mu,smu))
  A<-As[1]
  sA<-As[2]
  sigs<-roundfunc(c(mu,smu))
  sig<-sigs[1]
  ssig<-sigs[2]
  
  #for(i in 1:N){
    cat(paste(title,"&$",mu,"\\pm",smu,"$&$",sep=""))
    cat(paste(sig,"\\pm",smu,"$&$",sep=""))
    cat(paste(N,"\\pm",sN,"$\\\\\n",sep=""))
  #}

}

printfitdata <- function(fitdata,title=""){ #--- Ausgabe der Gaußfit-Daten
  
  mu<-fitdata["mu","Estimate"]
  smu<-fitdata["mu","Std. Error"]
  A<-fitdata["N","Estimate"]
  sA<-fitdata["N","Std. Error"]
  sig<-fitdata["sig","Estimate"]
  ssig<-fitdata["sig","Std. Error"]
  
  cat(title)
  cat("\n")
  
  cat(" mu    = ")
  cat(mu)
  cat("+-")
  cat(smu)
  cat("\n")
  
  cat(" sigma = ")
  cat(sig)
  cat("+-")
  cat(ssig)
  cat("\n")

  cat(" A     = ")
  cat(A*1/(sqrt(2*pi)*sig))
  cat("+-")
  cat(A*1/(sqrt(2*pi)*sig)*sqrt((sA/A)^2+(ssig/sig)^2))
  cat("\n")
  
}

getmu <- function(fitdata){
  mu<-fitdata["mu","Estimate"]
  smu<-fitdata["mu","Std. Error"]
  
  return(c(mu,smu))
}

getmus <- function(fitdata){
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  mu6<-fitdata["mu6","Estimate"]
  mu7<-fitdata["mu7","Estimate"]
  mu8<-fitdata["mu8","Estimate"]
  mu9<-fitdata["mu9","Estimate"]
  
  return(c(mu1,mu2,mu3,mu4,mu5,mu6,mu7,mu8,mu9))
}

getmus5 <- function(fitdata){
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]

  return(c(mu1,mu2,mu3,mu4,mu5))
}

getmus3 <- function(fitdata){
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]

  return(c(mu1,mu2,mu3))
}



getmuerrs <- function(fitdata){
  smu1<-fitdata["mu1","Std. Error"]
  smu2<-fitdata["mu2","Std. Error"]
  smu3<-fitdata["mu3","Std. Error"]
  smu4<-fitdata["mu4","Std. Error"]
  smu5<-fitdata["mu5","Std. Error"]
  smu6<-fitdata["mu6","Std. Error"]
  smu7<-fitdata["mu7","Std. Error"]
  smu8<-fitdata["mu8","Std. Error"]
  smu9<-fitdata["mu9","Std. Error"]
  
  return(c(smu1,smu2,smu3,smu4,smu5,smu6,smu7,smu8,smu9))
}

getmuerrs5 <- function(fitdata){
  smu1<-fitdata["mu1","Std. Error"]
  smu2<-fitdata["mu2","Std. Error"]
  smu3<-fitdata["mu3","Std. Error"]
  smu4<-fitdata["mu4","Std. Error"]
  smu5<-fitdata["mu5","Std. Error"]

  return(c(smu1,smu2,smu3,smu4,smu5))
}

getmuerrs3 <- function(fitdata){
  smu1<-fitdata["mu1","Std. Error"]
  smu2<-fitdata["mu2","Std. Error"]
  smu3<-fitdata["mu3","Std. Error"]

  return(c(smu1,smu2,smu3))
}




printninefitdata <- function(fitdata,title=""){ #--- Ausgabe der Gaußfit-Daten
  
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  mu6<-fitdata["mu6","Estimate"]
  mu7<-fitdata["mu7","Estimate"]
  mu8<-fitdata["mu8","Estimate"]
  mu9<-fitdata["mu9","Estimate"]
  smu1<-fitdata["mu1","Std. Error"]
  smu2<-fitdata["mu2","Std. Error"]
  smu3<-fitdata["mu3","Std. Error"]
  smu4<-fitdata["mu4","Std. Error"]
  smu5<-fitdata["mu5","Std. Error"]
  smu6<-fitdata["mu6","Std. Error"]
  smu7<-fitdata["mu7","Std. Error"]
  smu8<-fitdata["mu8","Std. Error"]
  smu9<-fitdata["mu9","Std. Error"]
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  N4<-fitdata["N4","Estimate"]
  N5<-fitdata["N5","Estimate"]
  N6<-fitdata["N6","Estimate"]
  N7<-fitdata["N7","Estimate"]
  N8<-fitdata["N8","Estimate"]
  N9<-fitdata["N9","Estimate"]
  sN1<-fitdata["N1","Std. Error"]
  sN2<-fitdata["N2","Std. Error"]
  sN3<-fitdata["N3","Std. Error"]
  sN4<-fitdata["N4","Std. Error"]
  sN5<-fitdata["N5","Std. Error"]
  sN6<-fitdata["N6","Std. Error"]
  sN7<-fitdata["N7","Std. Error"]
  sN8<-fitdata["N8","Std. Error"]
  sN9<-fitdata["N9","Std. Error"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  sig4<-fitdata["sig4","Estimate"]
  sig5<-fitdata["sig5","Estimate"]
  sig6<-fitdata["sig6","Estimate"]
  sig7<-fitdata["sig7","Estimate"]
  sig8<-fitdata["sig8","Estimate"]
  sig9<-fitdata["sig9","Estimate"]
  ssig1<-fitdata["sig1","Std. Error"]
  ssig2<-fitdata["sig2","Std. Error"]
  ssig3<-fitdata["sig3","Std. Error"]
  ssig4<-fitdata["sig4","Std. Error"]
  ssig5<-fitdata["sig5","Std. Error"]
  ssig6<-fitdata["sig6","Std. Error"]
  ssig7<-fitdata["sig7","Std. Error"]
  ssig8<-fitdata["sig8","Std. Error"]
  ssig9<-fitdata["sig9","Std. Error"]
  
  cat(title)
  cat("\n")
  
  cat(paste(" mu1    = ",mu1,"+-",smu1,"\n",sep=""))
  cat(paste(" mu2    = ",mu2,"+-",smu2,"\n",sep=""))
  cat(paste(" mu3    = ",mu3,"+-",smu3,"\n",sep=""))
  cat(paste(" mu4    = ",mu4,"+-",smu4,"\n",sep=""))
  cat(paste(" mu5    = ",mu5,"+-",smu5,"\n",sep=""))
  cat(paste(" mu6    = ",mu6,"+-",smu6,"\n",sep=""))
  cat(paste(" mu7    = ",mu7,"+-",smu7,"\n",sep=""))
  cat(paste(" mu8    = ",mu8,"+-",smu8,"\n",sep=""))
  cat(paste(" mu9    = ",mu9,"+-",smu9,"\n",sep=""))
  cat(paste(" sigma1    = ",sig1,"+-",ssig1,"\n",sep=""))
  cat(paste(" sigma2    = ",sig2,"+-",ssig2,"\n",sep=""))
  cat(paste(" sigma3    = ",sig3,"+-",ssig3,"\n",sep=""))
  cat(paste(" sigma4    = ",sig4,"+-",ssig4,"\n",sep=""))
  cat(paste(" sigma5    = ",sig5,"+-",ssig5,"\n",sep=""))
  cat(paste(" sigma6    = ",sig6,"+-",ssig6,"\n",sep=""))
  cat(paste(" sigma7    = ",sig7,"+-",ssig7,"\n",sep=""))
  cat(paste(" sigma8    = ",sig8,"+-",ssig8,"\n",sep=""))
  cat(paste(" sigma9    = ",sig9,"+-",ssig9,"\n",sep=""))
  cat(paste(" N1    = ",N1,"+-",sN1,"\n",sep=""))
  cat(paste(" N2    = ",N2,"+-",sN2,"\n",sep=""))
  cat(paste(" N3    = ",N3,"+-",sN3,"\n",sep=""))
  cat(paste(" N4    = ",N4,"+-",sN4,"\n",sep=""))
  cat(paste(" N5    = ",N5,"+-",sN5,"\n",sep=""))
  cat(paste(" N6    = ",N6,"+-",sN6,"\n",sep=""))
  cat(paste(" N7    = ",N7,"+-",sN7,"\n",sep=""))
  cat(paste(" N8    = ",N8,"+-",sN8,"\n",sep=""))
  cat(paste(" N9    = ",N9,"+-",sN9,"\n",sep=""))
}

printsevenfitdata <- function(fitdata,title=""){ #--- Ausgabe der Gaußfit-Daten
  
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  mu6<-fitdata["mu6","Estimate"]
  mu7<-fitdata["mu7","Estimate"]
  smu1<-fitdata["mu1","Std. Error"]
  smu2<-fitdata["mu2","Std. Error"]
  smu3<-fitdata["mu3","Std. Error"]
  smu4<-fitdata["mu4","Std. Error"]
  smu5<-fitdata["mu5","Std. Error"]
  smu6<-fitdata["mu6","Std. Error"]
  smu7<-fitdata["mu7","Std. Error"]
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  N4<-fitdata["N4","Estimate"]
  N5<-fitdata["N5","Estimate"]
  N6<-fitdata["N6","Estimate"]
  N7<-fitdata["N7","Estimate"]
  sN1<-fitdata["N1","Std. Error"]
  sN2<-fitdata["N2","Std. Error"]
  sN3<-fitdata["N3","Std. Error"]
  sN4<-fitdata["N4","Std. Error"]
  sN5<-fitdata["N5","Std. Error"]
  sN6<-fitdata["N6","Std. Error"]
  sN7<-fitdata["N7","Std. Error"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  sig4<-fitdata["sig4","Estimate"]
  sig5<-fitdata["sig5","Estimate"]
  sig6<-fitdata["sig6","Estimate"]
  sig7<-fitdata["sig7","Estimate"]
  ssig1<-fitdata["sig1","Std. Error"]
  ssig2<-fitdata["sig2","Std. Error"]
  ssig3<-fitdata["sig3","Std. Error"]
  ssig4<-fitdata["sig4","Std. Error"]
  ssig5<-fitdata["sig5","Std. Error"]
  ssig6<-fitdata["sig6","Std. Error"]
  ssig7<-fitdata["sig7","Std. Error"]

  cat(title)
  cat("\n")
  
  cat(paste(" mu1    = ",mu1,"+-",smu1,"\n",sep=""))
  cat(paste(" mu2    = ",mu2,"+-",smu2,"\n",sep=""))
  cat(paste(" mu3    = ",mu3,"+-",smu3,"\n",sep=""))
  cat(paste(" mu4    = ",mu4,"+-",smu4,"\n",sep=""))
  cat(paste(" mu5    = ",mu5,"+-",smu5,"\n",sep=""))
  cat(paste(" mu6    = ",mu6,"+-",smu6,"\n",sep=""))
  cat(paste(" mu7    = ",mu7,"+-",smu7,"\n",sep=""))
  cat(paste(" sigma1    = ",sig1,"+-",ssig1,"\n",sep=""))
  cat(paste(" sigma2    = ",sig2,"+-",ssig2,"\n",sep=""))
  cat(paste(" sigma3    = ",sig3,"+-",ssig3,"\n",sep=""))
  cat(paste(" sigma4    = ",sig4,"+-",ssig4,"\n",sep=""))
  cat(paste(" sigma5    = ",sig5,"+-",ssig5,"\n",sep=""))
  cat(paste(" sigma6    = ",sig6,"+-",ssig6,"\n",sep=""))
  cat(paste(" sigma7    = ",sig7,"+-",ssig7,"\n",sep=""))
  cat(paste(" N1    = ",N1,"+-",sN1,"\n",sep=""))
  cat(paste(" N2    = ",N2,"+-",sN2,"\n",sep=""))
  cat(paste(" N3    = ",N3,"+-",sN3,"\n",sep=""))
  cat(paste(" N4    = ",N4,"+-",sN4,"\n",sep=""))
  cat(paste(" N5    = ",N5,"+-",sN5,"\n",sep=""))
  cat(paste(" N6    = ",N6,"+-",sN6,"\n",sep=""))
  cat(paste(" N7    = ",N7,"+-",sN7,"\n",sep=""))
}

printfivefitdata <- function(fitdata,title=""){ #--- Ausgabe der Gaußfit-Daten
  
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  smu1<-fitdata["mu1","Std. Error"]
  smu2<-fitdata["mu2","Std. Error"]
  smu3<-fitdata["mu3","Std. Error"]
  smu4<-fitdata["mu4","Std. Error"]
  smu5<-fitdata["mu5","Std. Error"]
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  N4<-fitdata["N4","Estimate"]
  N5<-fitdata["N5","Estimate"]
  sN1<-fitdata["N1","Std. Error"]
  sN2<-fitdata["N2","Std. Error"]
  sN3<-fitdata["N3","Std. Error"]
  sN4<-fitdata["N4","Std. Error"]
  sN5<-fitdata["N5","Std. Error"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  sig4<-fitdata["sig4","Estimate"]
  sig5<-fitdata["sig5","Estimate"]
  ssig1<-fitdata["sig1","Std. Error"]
  ssig2<-fitdata["sig2","Std. Error"]
  ssig3<-fitdata["sig3","Std. Error"]
  ssig4<-fitdata["sig4","Std. Error"]
  ssig5<-fitdata["sig5","Std. Error"]

  cat(title)
  cat("\n")
  
  cat(paste(" mu1    = ",mu1,"+-",smu1,"\n",sep=""))
  cat(paste(" mu2    = ",mu2,"+-",smu2,"\n",sep=""))
  cat(paste(" mu3    = ",mu3,"+-",smu3,"\n",sep=""))
  cat(paste(" mu4    = ",mu4,"+-",smu4,"\n",sep=""))
  cat(paste(" mu5    = ",mu5,"+-",smu5,"\n",sep=""))
  cat(paste(" sigma1    = ",sig1,"+-",ssig1,"\n",sep=""))
  cat(paste(" sigma2    = ",sig2,"+-",ssig2,"\n",sep=""))
  cat(paste(" sigma3    = ",sig3,"+-",ssig3,"\n",sep=""))
  cat(paste(" sigma4    = ",sig4,"+-",ssig4,"\n",sep=""))
  cat(paste(" sigma5    = ",sig5,"+-",ssig5,"\n",sep=""))
  cat(paste(" N1    = ",N1,"+-",sN1,"\n",sep=""))
  cat(paste(" N2    = ",N2,"+-",sN2,"\n",sep=""))
  cat(paste(" N3    = ",N3,"+-",sN3,"\n",sep=""))
  cat(paste(" N4    = ",N4,"+-",sN4,"\n",sep=""))
  cat(paste(" N5    = ",N5,"+-",sN5,"\n",sep=""))
}

printthreefitdata <- function(fitdata,title=""){ #--- Ausgabe der Gaußfit-Daten
  
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  smu1<-fitdata["mu1","Std. Error"]
  smu2<-fitdata["mu2","Std. Error"]
  smu3<-fitdata["mu3","Std. Error"]
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  sN1<-fitdata["N1","Std. Error"]
  sN2<-fitdata["N2","Std. Error"]
  sN3<-fitdata["N3","Std. Error"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  ssig1<-fitdata["sig1","Std. Error"]
  ssig2<-fitdata["sig2","Std. Error"]
  ssig3<-fitdata["sig3","Std. Error"]

  cat(title)
  cat("\n")
  
  cat(paste(" mu1    = ",mu1,"+-",smu1,"\n",sep=""))
  cat(paste(" mu2    = ",mu2,"+-",smu2,"\n",sep=""))
  cat(paste(" mu3    = ",mu3,"+-",smu3,"\n",sep=""))
  cat(paste(" sigma1    = ",sig1,"+-",ssig1,"\n",sep=""))
  cat(paste(" sigma2    = ",sig2,"+-",ssig2,"\n",sep=""))
  cat(paste(" sigma3    = ",sig3,"+-",ssig3,"\n",sep=""))
  cat(paste(" N1    = ",N1,"+-",sN1,"\n",sep=""))
  cat(paste(" N2    = ",N2,"+-",sN2,"\n",sep=""))
  cat(paste(" N3    = ",N3,"+-",sN3,"\n",sep=""))
}

gausfunction <- function(fitdata,x){
  
  if(length(fitdata)==16){
    #print("1")
    a=onegausfunction(fitdata,x)
  }
  if(length(fitdata)==40){
    #print("3")
    a=threegausfunction(fitdata,x)
  }
  if(length(fitdata)==64){
    #print("5")
    a=fivegausfunction(fitdata,x)
  }
  if(length(fitdata)==88){
    #print("7")
    a=sevengausfunction(fitdata,x)
  }
  if(length(fitdata)==112){
    #print("9")
    a=ninegausfunction(fitdata,x)
  }
  return(a)
}

onegausfunction <- function(fitdata,x){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N<-fitdata["N","Estimate"]
  if(is.na(C))
    C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  sig<-fitdata["sig","Estimate"]
  
  #plot (function(x){C + N*1/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))},bereich[1],bereich[2],add=TRUE,col=col)
  
  return(C + N*1/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2)))
}


threegausfunction <- function(fitdata,x){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  C<-fitdata["C","Estimate"]
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  #print(x)
  #print(c(C,0))
  #plot (function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))},bereich[1],bereich[2],add=TRUE,col=col)
  #print(C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2)))
  return(C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2)))
}


fivegausfunction <- function(fitdata,x){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  N4<-fitdata["N4","Estimate"]
  N5<-fitdata["N5","Estimate"]
  C<-fitdata["C","Estimate"]
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  sig4<-fitdata["sig4","Estimate"]
  sig5<-fitdata["sig5","Estimate"]
  
  #plot (function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2))},bereich[1],bereich[2],add=TRUE,col=col)
  
  return(C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)))
}

sevengausfunction <- function(fitdata,x){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  N4<-fitdata["N4","Estimate"]
  N5<-fitdata["N5","Estimate"]
  N6<-fitdata["N6","Estimate"]
  N7<-fitdata["N7","Estimate"]
  C<-fitdata["C","Estimate"]
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  mu6<-fitdata["mu6","Estimate"]
  mu7<-fitdata["mu7","Estimate"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  sig4<-fitdata["sig4","Estimate"]
  sig5<-fitdata["sig5","Estimate"]
  sig6<-fitdata["sig6","Estimate"]
  sig7<-fitdata["sig7","Estimate"]
  
  #plot (function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2))},bereich[1],bereich[2],add=TRUE,col=col)
  
  return(C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2)))
}

ninegausfunction <- function(fitdata,x){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N1<-fitdata["N1","Estimate"]
  N2<-fitdata["N2","Estimate"]
  N3<-fitdata["N3","Estimate"]
  N4<-fitdata["N4","Estimate"]
  N5<-fitdata["N5","Estimate"]
  N6<-fitdata["N6","Estimate"]
  N7<-fitdata["N7","Estimate"]
  N8<-fitdata["N8","Estimate"]
  N9<-fitdata["N9","Estimate"]
  C<-fitdata["C","Estimate"]
  mu1<-fitdata["mu1","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  mu3<-fitdata["mu3","Estimate"]
  mu4<-fitdata["mu4","Estimate"]
  mu5<-fitdata["mu5","Estimate"]
  mu6<-fitdata["mu6","Estimate"]
  mu7<-fitdata["mu7","Estimate"]
  mu8<-fitdata["mu8","Estimate"]
  mu9<-fitdata["mu9","Estimate"]
  sig1<-fitdata["sig1","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  sig3<-fitdata["sig3","Estimate"]
  sig4<-fitdata["sig4","Estimate"]
  sig5<-fitdata["sig5","Estimate"]
  sig6<-fitdata["sig6","Estimate"]
  sig7<-fitdata["sig7","Estimate"]
  sig8<-fitdata["sig8","Estimate"]
  sig9<-fitdata["sig9","Estimate"]
  
  #plot (function(x){C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2)) + N8*1/(sqrt(2*pi)*sig8)*exp(-(x-mu8)^2/(2*sig8^2)) + N9*1/(sqrt(2*pi)*sig9)*exp(-(x-mu9)^2/(2*sig9^2))},bereich[1],bereich[2],add=TRUE,col=col)
  
  return(C + N1*1/(sqrt(2*pi)*sig1)*exp(-(x-mu1)^2/(2*sig1^2))+ N2*1/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))+ N3*1/(sqrt(2*pi)*sig3)*exp(-(x-mu3)^2/(2*sig3^2))+ N4*1/(sqrt(2*pi)*sig4)*exp(-(x-mu4)^2/(2*sig4^2)) + N5*1/(sqrt(2*pi)*sig5)*exp(-(x-mu5)^2/(2*sig5^2)) + N6*1/(sqrt(2*pi)*sig6)*exp(-(x-mu6)^2/(2*sig6^2)) + N7*1/(sqrt(2*pi)*sig7)*exp(-(x-mu7)^2/(2*sig7^2)) + N8*1/(sqrt(2*pi)*sig8)*exp(-(x-mu8)^2/(2*sig8^2)) + N9*1/(sqrt(2*pi)*sig9)*exp(-(x-mu9)^2/(2*sig9^2)))
}
