#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")

##Code for Confidence and Prediction Intervals Video
power <- read.table("power2015.txt",header=T)
attach(power)
head(power)

mod.power <- lm(Deadlift~Weight+Squat+Bench)
summary(mod.power)

##predict deadlift for Weight=46,Squat=120,Bench=73 (arbitrary values)
predict(mod.power,new=data.frame(Weight=46,Squat=120,Bench=73),interval="prediction")
predict(mod.power,new=data.frame(Weight=46,Squat=120,Bench=73),interval="confidence")