##Outliers Video
set.seed(123)
testdata <- data.frame(x=1:10,y=1:10+rnorm(10))
mod <- lm(y~x,testdata)

##Case 1
p1 <- c(5.5,12)
mod1 <- lm(y~x,rbind(testdata,p1))
plot(y~x,rbind(testdata,p1))
points(5.5,12,pch=4,cex=2)
abline(mod)
abline(mod1,lty=2)
#Compute standardized residuals using rstandard()
data.frame(round(rstandard(mod1),4))

#Match rstandard() output "by hand"
n <- dim(model.matrix(mod1))[1]
p <- dim(model.matrix(mod1))[2]
RSS <- sum(mod1$residuals^2)
sigma.hat <- sqrt(RSS/(n-p))
rstandard.match <- mod1$residuals/(sigma.hat*sqrt(1-hatvalues(mod1)))
data.frame(round(rstandard.match,4))

##Case 2
p2 <- c(15,15.1)
mod2 <- lm(y~x,rbind(testdata,p2))
plot(y~x,rbind(testdata,p2))
points(15,15.1,pch=4,cex=2)
abline(mod)
abline(mod2,lty=2)
data.frame(round(rstandard(mod2),4))

#Case 3
p3 <- c(15,5.1)
mod3 <- lm(y~x,rbind(testdata,p3))
plot(y~x,rbind(testdata,p3))
points(15,5.1,pch=4,cex=2)
abline(mod)
abline(mod3,lty=2)
data.frame(round(rstandard(mod3),4))




##Influence Video
set.seed(123)
testdata <- data.frame(x=1:10,y=1:10+rnorm(10))
mod <- lm(y~x,testdata)

##Case 1
p1 <- c(5.5,12)
mod1 <- lm(y~x,rbind(testdata,p1))
plot(y~x,rbind(testdata,p1))
points(5.5,12,pch=4,cex=2)
abline(mod)
abline(mod1,lty=2)
#Find threshold for Cooks distance, 50th percentile of F distribution with (p+1) and n-(p+1) numerator and denominator DF
n <- dim(model.matrix(mod1))[1]
p <- dim(model.matrix(mod1))[2]
num.df <- p
den.df <- n-p
F.thresh <- qf(0.5,num.df,den.df)
F.thresh
#calculate Cook's distances
cooks.distance(mod1)
which(cooks.distance(mod1)>F.thresh)

##Case 2
p2 <- c(15,15.1)
mod2 <- lm(y~x,rbind(testdata,p2))
plot(y~x,rbind(testdata,p2))
points(15,15.1,pch=4,cex=2)
abline(mod)
abline(mod2,lty=2)
cooks.distance(mod2)
which(cooks.distance(mod2)>F.thresh)

##Case 3
p3 <- c(15,5.1)
mod3 <- lm(y~x,rbind(testdata,p3))
plot(y~x,rbind(testdata,p3))
points(15,5.1,pch=4,cex=2)
abline(mod)
abline(mod3,lty=2)
cooks.distance(mod3)
which(cooks.distance(mod3)>F.thresh)

