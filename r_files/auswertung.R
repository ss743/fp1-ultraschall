data = read.csv("../data/Referenzgitter2.0_HM1508-2.csv")

time=data$Time
y=data$CH.A

plot(time,y,type="p",pch=4,cex=0.6,bty="l")
grid()