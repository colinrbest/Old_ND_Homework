setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")

## "Applying Goodness of Fit Measures" Video
baseball <- read.table("Baseball2011.txt",header=T)
attach(baseball)
dim(baseball)
head(baseball)

BA <- H/AB
OBP <- (H+BB+HBP)/(AB+BB+HBP+SF)
Win.p <- W/G
mod1 <- lm(Win.p~BA)
summary(mod1)
mod2 <- lm(Win.p~OBP)
summary(mod2)