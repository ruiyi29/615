#11.5a
setwd("~/Documents/BU/2022 Fall/678/ROS-Examples-master/pyth")
pyth <- read.table('pyth.txt',header=T)
m1 <- lm(y~x1+x2,data=pyth[1:40,])
summary(m1)

#11.5b
plot(pyth$x1[1:40],pyth$y[1:40],pch=20,xlab='x1',ylab='y',cex=1)
abline(coef(m1)[1]+coef(m1)[3]*mean(pyth$x2[1:40]),coef(m1)[2])
plot(pyth$x2[1:40],pyth$y[1:40],pch=20,xlab='x2',ylab='y',cex=1)
abline(coef(m1)[1]+coef(m1)[2]*mean(pyth$x1[1:40]),coef(m1)[3])

#11.5c
plot(m1,which=1,cex=1)
plot(m1,which=2,cex=1)
## Independence is met, but variance and normality are not met.

#11.5d
a <- data.frame(x1=pyth$x1[41:60],x2=pyth$x2[41:60])
predict(m1,a)
## R square is large enough so very confident.

#12.5a
