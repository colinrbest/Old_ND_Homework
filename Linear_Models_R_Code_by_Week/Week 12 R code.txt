##Box-Cox Video
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")
data <- read.table("teengamb.txt",header=T)
attach(data)
head(data)

#Fit initial model with expenditure on gambling as response and other variables as predictors
mod <- lm((gamble+1) ~ sex + income + verbal)
summary(mod)
plot(mod$fitted.values,mod$residuals)
###zoom out on plot
plot(mod$fitted.values,mod$residuals,ylim=c(-150,150),xlim=c(-100,100))

#Try Box-Cox to remove heteroscedasticity
require(MASS)
bc <- boxcox(mod, plotit=T)
lambda <- bc$x[which.max(bc$y)]
lambda
mod.bc <- lm(log(gamble+1) ~ sex + income + verbal)
summary(mod.bc)
plot(mod.bc$fitted.values,mod.bc$residuals,ylim=c(-5,5),xlim=c(0,8))



## Transform endpoints Video
predict(mod.bc,new=data.frame(sex=0,income=median(income),verbal=median(verbal)),interval="confidence")
exp(1.758943)-1
exp(2.683797)-1



##Box-Cox and Polynomials Video
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")
library(faraway)
data <- data.frame(savings)
attach(data)
head(data)

#fit linear model 
plot(ddpi,sr)
mod1 <- lm(sr ~ ddpi)
summary(mod1)
plot(mod1$fitted.values,mod1$residuals)
plot(ddpi,sr)
abline(mod1)
#Compute Cook's Distances and F statistic threshold
cooks.distance(mod1)
summary(cooks.distance(mod1))
n <- dim(model.matrix(mod1))[1]
p <- dim(model.matrix(mod1))[2]
num.df <- p
den.df <- n-p
F.thresh <- qf(0.5,num.df,den.df)
F.thresh
which(cooks.distance(mod1)>F.thresh)
#Make plot with influential point marked red
plot(ddpi,sr,col=ifelse(ddpi==ddpi[49], "red", "black"))
abline(mod1)

#fit model with polynomial term
mod2 <- lm(sr ~ ddpi+I(ddpi^2))
summary(mod2)
plot(mod2$fitted.values,mod2$residuals)

#view fit of polynomial (abline doesn't work with polynomials)
plot(ddpi,sr)
abline(mod1)
x.range <- seq(from=0,to=20,by=0.001)
fitted.poly <- cbind(1,x.range,x.range^2)%*%coef(mod2)
points(x.range,fitted.poly)

#######Try Box-Cox on same data instead of polynomial
#######
require(MASS)
bc <- boxcox(mod1, plotit=T)
lambda <- bc$x[which.max(bc$y)]
lambda
mod.bc <- lm(sr^lambda ~ ddpi)
summary(mod.bc)
plot(mod.bc$fitted.values,mod.bc$residuals)

##Slight digression: What does the I() function do?
#Attempt to fit polynomial without I() function
mod.alt1 <- lm(sr ~ ddpi+ddpi^2)
summary(mod.alt1) #Compare to summary for mod2 above
