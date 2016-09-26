g <- 2*pi/0.12926
g1 <- 2*pi/0.12926 
g2 <- 2*pi/0.12926
apertur <- function(x){sqrt(2.72)+sqrt(0.935)*cos(x*g)+ sqrt(0.6735)*cos(2*x*g)+sqrt(0.4245)*cos(3*x*g)+sqrt(0.1815)*cos(4*x*g)+sqrt(0.04012)*cos(5*x*g)}
apertur1 <- function(x){sqrt(2.66)+sqrt(0.921)*cos(x*g1)+ sqrt(0.6035)*cos(2*x*g1)+sqrt(0.4175)*cos(3*x*g1)+sqrt(0.1755)*cos(4*x*g1)+sqrt(0.04004)*cos(5*x*g1)}
apertur2 <- function(x){sqrt(2.78)+sqrt(0.949)*cos(x*g2)+ sqrt(0.7435)*cos(2*x*g2)+sqrt(0.4315)*cos(3*x*g2)+sqrt(0.1875)*cos(4*x*g2)+sqrt(0.04020)*cos(5*x*g2)}
plot (apertur,-0.05,0.05)
max0 <- sqrt(2.72)+sqrt(0.935)+ sqrt(0.6735)+sqrt(0.4245)+sqrt(0.1815)+sqrt(0.04012)
max1 <- sqrt(2.66)+sqrt(0.921)+ sqrt(0.6035)+sqrt(0.4175)+sqrt(0.1755)+sqrt(0.04004)
max2 <- sqrt(2.78)+sqrt(0.949)+ sqrt(0.7435)+sqrt(0.4315)+sqrt(0.1875)+sqrt(0.04020)

f0 <- function(x){(max0/2)*x^0}
f1 <- function(x){(max1/2)*x^0}
f2 <- function(x){(max2/2)*x^0}

interf <- function(x){abs(apertur(x)-f0(x))}
interf1 <- function(x){abs(apertur1(x)-f1(x))} 
interf2 <- function(x){abs(apertur2(x)-f2(x))}
intersect <- optimize(interf, interval=c(-0.02, 0))
intersect1 <- optimize(interf1, interval=c(-0.02, 0))
intersect2 <- optimize(interf2, interval=c(-0.02, 0))

plot(f0, -0.05,0.05, add =  TRUE)
plot(f1, -0.05,0.05, add =  TRUE, col = "red" )
plot(f2, -0.05,0.05, add =  TRUE, col= "blue")
plot(apertur1, -0.05,0.05, add =  TRUE, col = "red")
plot(apertur2, -0.05,0.05, add =  TRUE, col= "blue")
#plot(interf, -0.2,0.0)