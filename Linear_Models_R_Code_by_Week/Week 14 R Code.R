##Lasso for Simulated Data Video
library(glmnet)
##Create RMSE function
rmse <- function(x,y) sqrt(mean((x-y)^2))
#Set seed for reproducible results
set.seed(3)
#Set sample sizes and true, or generating model parameters
n <- 100
BETA0 <- 3
#Generate 30 coefficients, 10 big and 20 zero
BETAS <- c(runif(10,0.5,1.0),rep(0,20))
#Generate p=30 predictors
num.pred <- length(BETAS) #number of predictors
X <- matrix(rnorm(n*num.pred),nrow=n,ncol=num.pred)
y <- BETA0 + X%*%BETAS + rnorm(n)

##Split data into training and test samples
#Use the sample and setdiff functions to obtain the indices of observations in each group
train.index <-sample(1:n,round(n/2),replace=F)
test.index <-setdiff(1:n,train.index)
#Use the indices to define the training and test samples
X.train <- X[train.index,]; y.train <- y[train.index]
X.test <- X[test.index,]; y.test <- y[test.index]

# Fit the ridge regression model (ridge: Alpha = 0)
mod.ridge <- cv.glmnet(X.train, y.train, alpha=0)
# Results
plot(mod.ridge)
mod.ridge$lambda.min
#Obtain test predicted values for both values of lambda for ridge regression
ypred.ridge.min <- cbind(1,X.test)%*%coef(mod.ridge, s=mod.ridge$lambda.min)

#Lasso (lasso: alpha = 1)
mod.lasso <- cv.glmnet(X.train, y.train, alpha=1)
# Results
plot(mod.lasso)
mod.lasso$lambda.min
#Obtain test predicted values for both values of lambda for lasso
ypred.lasso.min <- cbind(1,X.test)%*%coef(mod.lasso,s=mod.lasso$lambda.min)

#Fit OLS
mod.ols <- lm(y.train~X.train)
#Obtain test predicted values for OLS
ypred.ols <- cbind(1,X.test)%*%coef(mod.ols)

#Compare RMSE for all methods
#Ridge
rmse(ypred.ridge.min,y.test)
#Lasso
rmse(ypred.lasso.min,y.test)
#OLS
rmse(ypred.ols,y.test)

#Examine lasso estimates under both values of lambda
coef(mod.lasso,s=mod.lasso$lambda.min)



##Lasso for Song Release Year Video
#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")
data <- read.table("YearPredictionMSD.txt")
dim(data)
n <- dim(data)[1]
#Create function to compute RMSE
rmse <- function(x,y) sqrt(mean((x-y)^2))
#Specify first column "Year" as 
X <- as.matrix(data[,-1])
y <- data[,1]
#Use the sample and setdiff functions to obtain the indices of observations in each group
train.index <-sample(1:n,round(n/2),replace=F)
test.index <-setdiff(1:n,train.index)
#Now, subset the data into the training and validation parts
X.train <- X[train.index,]; y.train <- y[train.index]
X.test <- X[test.index,]; y.test <- y[test.index]

# Fit Ridge Regression model (ridge: alpha = 0)
library(glmnet)
mod.ridge <- cv.glmnet(X.train, y.train, alpha=0)
# Results
plot(mod.ridge)
mod.ridge$lambda.min
log(mod.ridge$lambda.min)
#Obtain test predicted values from ridge regression model
ypred.ridge.min <- cbind(1,X.test)%*%coef(mod.ridge,mod.ridge$lambda.min)

#Lasso (lasso: alpha = 1)
mod.lasso <- cv.glmnet(X.train, y.train, alpha=1)
# Results
plot(mod.lasso)
mod.lasso$lambda.min
#Obtain test predicted values for lasso
ypred.lasso.min <- cbind(1,X.test)%*%coef(mod.lasso,s=mod.lasso$lambda.min)

#Fit OLS
mod.ols <- lm(y.train~X.train)
#Obtain predicted values from OLS
ypred.ols <- cbind(1,X.test)%*%coef(mod.ols)

#Compare RMSE for all methods
#Ridge
rmse(ypred.ridge.min,y.test)
#Lasso
rmse(ypred.lasso.min,y.test)
#OLS
rmse(ypred.ols,y.test)

#Examine lasso estimates 
coef(mod.lasso,s=mod.lasso$lambda.min)