library("Hmisc")
y1 <- c(0, 0.7072, -0.7091, 1.4171, -1.4126)*10^-4
x <- c(0, 0.005, -0.005, 0.01, -0.01)

erry1 <- c(0.0004, 0.0007, 0.0007, 0.0017, 0.0015)*10^-4


plot(x, y1, type="p", xlim=c(-0.01, 0.01), ylim=c(-0.0002, 0.0002), xlab = expression(sin(theta)), ylab = "time/ s", pch=4)
with (
  data = data.frame(x,y1,erry1)
  , expr = errbar(x, y1, y1+erry1, y1-erry1, type="n",add=T, pch=1, cap=.015)
)


text(x=-0.0001, y=0.0001, "y=(0.014155+-0.000013)s*x-(2+-5*10^-8)s", cex=0.8)
fm1 <- lm(y1 ~ x, weights=1/erry1^2)
abline(fm1, col = "red")

slope=summary(fm1)$coefficients["x","Estimate"]
sslope=summary(fm1)$coefficients["x","Std. Error"]
a=1/slope
sa=sslope/slope*a


#*******************************************************************************
