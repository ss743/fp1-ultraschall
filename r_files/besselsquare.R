besselsquarefit <- function(data,m){
  
  besselsquare <- y ~ (besselJ(alpha*x,m))^2
  
  alpha0=0.16
  
  #plot(function(x){(besselJ(x*alpha0,m))^2},0,10,col="black",add=TRUE)
  
  fit = nls(besselsquare,data,start=list(alpha=alpha0))
  
  return(summary(fit)$parameters)
}