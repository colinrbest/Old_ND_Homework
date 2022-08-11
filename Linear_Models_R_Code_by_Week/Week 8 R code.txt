#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")

##Fast BW and Adjusted R-squared Videos
data1 <- read.table("mpg.txt",header=T)
data1 <- na.omit(data1)
attach(data1)

#Fit model with all 6 predictors
mod.mpg <- lm(MPG~cylinders+displacement+horsepower+weight+acceleration+year)
summary(mod.mpg)

#Fit above model using ols() function in order to use fastbw() function
require(rms)
ols.mpg <- ols(MPG~cylinders+displacement+horsepower+weight+acceleration+year)
#Perform p-value based selection using fastbw() function
fastbw(ols.mpg, rule = "p", sls = 0.05)

#Fit reduced model 
mod2.mpg <- lm(MPG~weight+year)
summary(mod2.mpg)


