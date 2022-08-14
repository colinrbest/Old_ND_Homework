#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")

#Read in data
white <- read.csv("winequality-white.csv",header=T,sep=";")
attach(white)
mod.white <- lm(quality~volatile.acidity+sulphates+alcohol)
summary(mod.white)

#Check assumption of constant error variance
plot(mod.white$fitted.values,mod.white$residuals)

#Check assumption of normal errors
hist(mod.white$residuals)
qqnorm(mod.white$residuals)
shapiro.test(mod.white$residuals)

#Check assumption of independent errors
n <- dim(white)[1]
plot(mod.white$residuals[1:(n-1)],mod.white$residuals[2:n])
require(lmtest)
dwtest(mod.white)
