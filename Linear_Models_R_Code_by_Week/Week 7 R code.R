#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")
data <- read.table("Baseball2011.txt",header=T)
attach(data)

##Intro to Multicollinearity Video
#Define baseball statistics
BA <- H/AB
OBP <- (H+BB+HBP)/(AB+BB+HBP+SF)
X1B <- H-(X2B+X3B+HR)
SLG <- (X1B+2*X2B+3*X3B+4*HR)/AB
Win.p <- W/G

#Fit four simple linear regression models predicting Win.p
mod1 <- lm(Win.p~BA)
mod2 <- lm(Win.p~OBP)
mod3 <- lm(Win.p~SLG)
mod4 <- lm(Win.p~HR)

#View summaries
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

#Fit model with all four predictors
mod5 <- lm(Win.p ~ BA+OBP+HR+SLG)
summary(mod5)



##Correlation Video
cor.predictors <- cor(cbind(BA,OBP,SLG,HR))
cor.predictors



##Eigenvalues Video
X <- model.matrix(mod5)[,-1]
e <- eigen(t(X) %*% X)
e$val
#compute kappa measures of relative sizes
sqrt(e$val[1]/e$val)

 
 
###Variance Inflation Factors Video
mod.OBP <- lm(OBP ~ BA+SLG+HR)
R2.OBP <- summary(mod.OBP)$r.squared
VIF.OBP <- 1/(1-R2.OBP)
VIF.OBP
#Check using faraway package
require(faraway)
X <- model.matrix(mod5)[,-1]
vif(X)
 