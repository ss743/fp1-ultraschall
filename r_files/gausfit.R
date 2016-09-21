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

#gausfit <- function(input,weighted=FALSE,sig0=0,N0=0){
# 
#  bereich=
#  gausfit(input,bereich,weighted,sig0,N0)
#}

#gausfit <- function(x,y,weighted=FALSE,sig0=0,N0=0,sy=0){
  
#  bereich=c(x[1],x[length(x)])
#  gausfit(x,y,bereich,weighted,sig0,N0,sy)
#}


plotgaus <- function(fitdata,bereich){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  
  N<-fitdata["N","Estimate"]
  C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  sig<-fitdata["sig","Estimate"]
  
  plot (function(x){C + N*1/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))},bereich[1],bereich[2],add=TRUE,col="red")
  
}

texfitdata <- function(fitdata,title=""){ #--- Ausgabe der Gaußfit-Daten
  
  mu<-fitdata["mu","Estimate"]
  smu<-fitdata["mu","Std. Error"]
  A<-fitdata["N","Estimate"]
  sA<-fitdata["N","Std. Error"]
  sig<-fitdata["sig","Estimate"]
  ssig<-fitdata["sig","Std. Error"]
  
  cat(paste("\\text{",title,"}",sep=""))
  cat("\\\\\n")
  
  cat(" \\mu = ")
  cat(mu)
  cat("\\pm")
  cat(smu)
  cat("\\\\\n")
  
  cat(" \\sigma = ")
  cat(sig)
  cat("\\pm")
  cat(ssig)
  cat("\\\\\n")
  
  cat(" A = ")
  cat(A*1/(sqrt(2*pi)*sig))
  cat("\\pm")
  cat(A*1/(sqrt(2*pi)*sig)*sqrt((sA/A)^2+(ssig/sig)^2))
  cat("\\\\\n")
  
  
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

