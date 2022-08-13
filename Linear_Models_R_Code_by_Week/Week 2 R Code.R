#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")

##Fitted Values and Residuals for Multiple Regression Video
bp <- read.table("BreadPeace.txt",header=T)
attach(bp)

mod <- lm(vote~growth+killed)
summary(mod)

#match with matrix formula
X <- model.matrix(mod)
X
my.beta.hat <- solve(t(X)%*%X)%*%t(X)%*%vote
my.beta.hat
