##Ridge Regression Simulated Data Video
library(glmnet)
##Create function to compute RMSE
rmse <- function(x,y) sqrt(mean((x-y)^2))
#Set seed for reproducible results
set.seed(3)
#Set sample sizes and true, or generating model parameters
n <- 100
BETA0 <- 3
#Generate 15 coefficients
#5 "big" (between 0.5 and 1) and 10 "small"(=0.1)
BETAS <- c(runif(5,0.5,1.0),rep(0.1,10))
#Generate 15 predictors from standard normal disbtribution
num.x <- length(BETAS) #number of predictors in model
X <- matrix(rnorm(n*num.x),nrow=n,ncol=num.x)
y <- BETA0 + X%*%BETAS + rnorm(n)
head(data.frame(y,X))

##Split data into training and test samples
#Use the sample and setdiff functions to obtain the indices of observations in each group
train.index <-sample(1:n,round(n/2),replace=F)
test.index <-setdiff(1:n,train.index)
head(train.index)
head(test.index)
#Use the indices to define the training and test samples
X.train <- X[train.index,]; y.train <- y[train.index]
X.test <- X[test.index,]; y.test <- y[test.index]

# Fitting the model (Ridge: alpha = 0)
mod.ridge <- cv.glmnet(X.train, y.train, alpha=0)
# Results
plot(mod.ridge)
mod.ridge$lambda.min
mod.ridge$lambda.1se
log(mod.ridge$lambda.min)
log(mod.ridge$lambda.1se)
#Obtain test predicted values from ridge regression model
ypred.ridge.min <- cbind(1,X.test)%*%coef(mod.ridge,mod.ridge$lambda.min)
ypred.ridge.1se <- cbind(1,X.test)%*%coef(mod.ridge,mod.ridge$lambda.1se)
#Compute RMSE for test data under ridge regression
rmse(ypred.ridge.min,y.test)
rmse(ypred.ridge.1se,y.test)

#Compare to ols
mod.ols <- lm(y.train~X.train)
#Obtain predicted values from OLS
ypred.ols <- cbind(1,X.test)%*%coef(mod.ols)
#Compute RMSE for test data under OLS
rmse(ypred.ols,y.test)
summary(mod.ols)

#Compare estimates for both ridge regression models and OLS
cbind(coef(mod.ridge,mod.ridge$lambda.1se),coef(mod.ridge,mod.ridge$lambda.min),coef(mod.ols))




##Ridge Regression Song Release Year Video
#Set location from which you are reading files
setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")
library(glmnet)
songs <- read.table("YearPredictionMSD.txt")
dim(songs)
n <- dim(songs)[1]
head(songs)
#Specify first column "Year" as response and all other columns as predictors  
y <- songs[,1]
X <- as.matrix(songs[,-1])
#Set seed for reproducible results
set.seed(3)
#enter rmse function to compare prediction for ridge regression and ols
rmse <- function(x,y) sqrt(mean((x-y)^2))

#Use the sample and setdiff functions to obtain the indices of observations in each group
train.index <-sample(1:n,round(n/2),replace=F)
test.index <-setdiff(1:n,train.index)
#Now, subset the data into the training and validation parts
X.train <- X[train.index,]; y.train <- y[train.index]
X.test <- X[test.index,]; y.test <- y[test.index]

# Fit Ridge Regression model (Ridge: alpha = 0)
require(glmnet)
mod.ridge <- cv.glmnet(X.train, y.train, alpha=0)
# Results
plot(mod.ridge)
mod.ridge$lambda.min
mod.ridge$lambda.1se
log(mod.ridge$lambda.min)
log(mod.ridge$lambda.1se)
#Obtain test predicted values from ridge regression model
ypred.ridge.min <- cbind(1,X.test)%*%coef(mod.ridge,mod.ridge$lambda.min)
ypred.ridge.1se <- cbind(1,X.test)%*%coef(mod.ridge,mod.ridge$lambda.1se)
#Compute RMSE for test data under ridge regression
rmse(ypred.ridge.min,y.test)
rmse(ypred.ridge.1se,y.test)

#Compare to ols
mod.ols <- lm(y.train~X.train)
#Obtain predicted values from OLS
ypred.ols <- cbind(1,X.test)%*%coef(mod.ols)
#Compute RMSE for test data under OLS
rmse(ypred.ols,y.test)
#look at summary for OLS model)
summary(mod.ols)

