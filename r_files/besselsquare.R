besselsquarefit <- function(data,m,weighted=FALSE){
  
  besselsquare <- y ~ (besselJ(alpha*x,m))^2
  
  alpha0=0.25
  
  #plot(function(x){(besselJ(x*alpha0,m))^2},0,10,col="black",add=TRUE)
  if(weighted){
    err=data$sy
    fit = nls(besselsquare,data,start=list(alpha=alpha0),weights=1/err^2)
  } else {
    fit = nls(besselsquare,data,start=list(alpha=alpha0))
  }
  
  return(summary(fit)$parameters)
}