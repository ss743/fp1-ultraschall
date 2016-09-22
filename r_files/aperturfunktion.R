g <- 2*pi/0.000000807
apertur <- function(x){sqrt(2.72)+sqrt(0.935)*cos(x*g)+ sqrt(0.6735)*cos(2*x*g)+sqrt(0.4245)*cos(3*x*g)+sqrt(0.1815)*cos(4*x*g)+sqrt(0.04012)*cos(5*x*g)}
plot (apertur,-0.0000002,0.000001)
max <- sqrt(2.72)+sqrt(0.935)+ sqrt(0.6735)+sqrt(0.4245)+sqrt(0.1815)+sqrt(0.06)
f1 <- function(x){(4.75938/2)*x^0}
interf <- function(x){abs(apertur(x)-f1(x))}
intersect <- optimize(interf, interval=c(-0.06, 0))
#plot(f1, -0.000002,0.000002, add =  TRUE)
#plot(interf, -0.000002,0.000002)