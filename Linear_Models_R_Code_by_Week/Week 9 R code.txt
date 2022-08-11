#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")

data1 <- read.table("mpg.txt",header=T)
data1 <- na.omit(data1)
attach(data1)

mod.mpg <- lm(MPG~cylinders+displacement+horsepower+weight+acceleration+year)
summary(mod.mpg)

extractAIC(mod.mpg)

#####Use stepAIC() function
library(MASS)
aic.mpg <- stepAIC(mod.mpg)
aic.mpg$anova # display results 

