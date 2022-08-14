setwd("C:/Users/ahuebne2/Dropbox/Online Data Science MS/data")

##Binary Predictors Video
data <- read.csv("dataset_Facebook.csv",header=T,sep=";")
data <- na.omit(data)
attach(data)
Paid <- as.factor(Paid)
mod1 <- lm(Lifetime.Post.Total.Reach ~ Paid)
summary(mod1)

#Change reference level 
Paid.new<-relevel(Paid, ref="1")
mod2 <- lm(Lifetime.Post.Total.Reach ~ Paid.new)
summary(mod2)



##Multi-Category Predictors Video
data <- read.csv("dataset_Facebook.csv",header=T,sep=";")
data <- na.omit(data)
attach(data)
levels(Type)
mod <- lm(Lifetime.Post.Total.Reach ~ Type)
summary(mod)

#Change reference level 
Type.new<-relevel(Type, ref="Video")
mod2 <- lm(Lifetime.Post.Total.Reach ~ Type.new)
summary(mod2)



##Statistical Interactions Video
#Read in data from separate files for white and red
white <- read.csv("winequality-white.csv",header=T,sep=";")
red <- read.csv("winequality-red.csv",header=T,sep=";")

#Obtain sample size for each type
n.red <- dim(red)[1]
n.white <- dim(white)[1]

#Add variable indicating wine type, 1=red, 0=white
white <- cbind(white,rep(0,n.white))
names(white)[13] <- "Type"
red <- cbind(red,rep(1,n.red))
names(red)[13] <- "Type"

#Combine into single data set
wine.all <- rbind(white,red)
attach(wine.all)

#Fit initial model without interaction
mod <- lm(quality~alcohol+Type)
summary(mod)

#Fit model with interaction
mod.int <- lm(quality~alcohol+Type+alcohol*Type)
summary(mod.int)

