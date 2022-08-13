#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")

## "Test for a Regression Slope" Video
power <- read.table("power2015.txt",header=T)
attach(power)
head(power)
mod.power <- lm(Deadlift~Weight+Squat+Bench)
summary(mod.power)

## "Introducing F-tests"
#Fit full model
mod.full <- lm(Deadlift~Weight+Squat+Bench)
summary(mod.full)

#Fit reduced model
mod.reduced <- lm(Deadlift~Squat)

#Use anova() function to perform F-test
anova(mod.reduced,mod.full)


#Model Utility Test
mod.null <- lm(Deadlift~1)
anova(mod.null,mod.full)
