library("Hmisc")
y1 <- c(0, 0.6729, -0.6719, 1.3479, -1.3427)*10^-4
x <- c(0, 0.005, -0.005, 0.01, -0.01)

erry1 <- c(3*10^-4, 0.0005, 0.0005, 0.0012, 0.0011)*10^-4


plot(x, y1, type="p", xlim=c(-0.01, 0.01), ylim=c(-0.0002, 0.0002), xlab = expression(sin(theta)), ylab = "time/ s", pch=4)
with (
  data = data.frame(x,y1,erry1)
  , expr = errbar(x, y1, y1+erry1, y1-erry1, type="n",add=T, pch=1, cap=.015)
)


text(x=-0.0001, y=0.0001, "y=(0.013449+-0.000009)s*x-(4+-4*10^-8)s", cex=0.8)
fm1 <- lm(y1 ~ x, weights=1/erry1^2)
abline(fm1, col = "red")

slope=summary(fm1)$coefficients["x","Estimate"]
sslope=summary(fm1)$coefficients["x","Std. Error"]
a=1/slope
sa=sslope/slope*a


#*******************************************************************************
