library("Hmisc")
y1 <- c(0, 0.6729, -0.6729, 1.3479, -1.3427)*10^-4
x <- c(0, 7.91*10^-5, -7.91*10^-5, 1.582*10^-4, -1.582*10^-4)

erry1 <- c(3*10^-4, 0.0005, 0.0005, 0.0012, 0.0011)*10^-4


plot(x, y1, type="p", xlim=c(-0.0002, 0.0002), ylim=c(-0.0002, 0.0002), xlab = expression(sin(theta)), ylab = "time/ s", pch=4)
with (
  data = data.frame(x,y1,erry1)
  , expr = errbar(x, y1, y1+erry1, y1-erry1, type="n",add=T, pch=1, cap=.015)
)


text(x=-0.0001, y=0.0001, "y=(0.8502+-0.0004)s*x-(2*10^-8+-4*10^-8)s", cex=0.8)
fm1 <- lm(y1 ~ x, weights=1/erry1^2)
summary(fm1)
abline(fm1, col = "red")


#*******************************************************************************
