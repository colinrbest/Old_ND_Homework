#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")

##Exploratory Data Analysis Video
bp <- read.table("BreadPeace.txt",header=T)
attach(bp)
#obtain numerical summaries
summary(bp)
#Look at histograms and scatter plots
par(mfrow=c(2,2))
hist(growth)
hist(killed)
plot(growth,vote)
plot(killed,vote)

##Fitting a Linear Model Video
#Fit simple linear model
mod1 <- lm(vote~growth)
summary(mod1)
#View regression line over scatterplot
#Use par() to view one plot rather than 2x2 above
par(mfrow=c(1,1))
plot(growth,vote)
abline(mod1)

##Fitted Values and Residuals Video
#View and access attributes of the linear model
attributes(mod1)
mod1$fitted.values
mod1$residuals
round(mod1$fitted.values,3)