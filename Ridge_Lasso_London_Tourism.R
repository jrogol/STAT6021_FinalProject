
library(glmnet)
library(readr)
library(zoo)
library(MASS)
library(DataCombine)
#Load and Clean Data
data <- read.csv('raw_with_avg_added_final.csv')
data$market <- as.factor(data$market)
data$duration <- factor(data$duration, levels=c("1-3  nights", "4-7  nights", "8-14 nights", "15+  nights"), ordered=TRUE)
data$method <- as.factor(data$method)
data$purpose <- as.factor(data$purpose)
data$date <- as.yearqtr(paste(data$Quarter, data$Year, sep = ' '), format = 'Q%q %Y')
data$Quarter <- factor(data$Quarter, levels = c("Q1", "Q2", "Q3", "Q4"), ordered = TRUE)

#Remove Unnecessary Variables
data <- data[, -c(1,8,9)]

#Transform Response Variable by Taking the Log set values that are 0 equal to 0.0000001

i=0
for (i in 1:nrow(data)){
  if (data$SpendPerVisit[i] == 0){
    data$SpendPerVisit[i] = 0.0000001
  }
}


##################
#Lasso Regression#
##################

# Create a vector of lambdas to test, from very large to very small.
grid <- 10^seq(10, -10, length = 100)

# Create Training and Testing Data, Testing set is 2015
train.data<- data[data$Year != '2015',]
test.data<- data[data$Year == '2015',]

#Create Matrixes

train.matirx<-model.matrix(log(SpendPerVisit)~., data = train.data)
test.matrix<- model.matrix(log(SpendPerVisit)~., data = test.data)

# Create a lasso model on the response variable, spend
lasso.mod <- glmnet(train.matirx,log(train.data$SpendPerVisit), alpha =1, lambda = grid, thresh = 1e-12)

# Set the seed for reproducable results
set.seed(21)

# 10-fold cross-validated lasso regression
lasso.out <- cv.glmnet(train.matirx, log(train.data$SpendPerVisit), lambda = grid, alpha = 1)
plot(lasso.out)

# find the best lambda
bestlam <- lasso.out$lambda.min

# predict using the best lambda and re transform prediction
lasso.predicts <- predict(lasso.mod, s = bestlam, newx = test.matrix)

#Take the Mean of the residuals  for the MSE
MSE<-mean((exp(lasso.predicts))-test.data$SpendPerVisit)^2 #116,806


#Coefficients of the model
coef(lasso.out, id = which.min(lasso.out$lambda))

#Calculate R^2

#R^2 = SSR/SST
SSR.lasso = sum((exp(lasso.predicts)- mean(test.data$SpendPerVisit))^2)
SST.lasso = sum((data$SpendPerVisit - mean(test.data$SpendPerVisit))^2)
R.squared.lasso = SSR.lasso/SST.lasso #0.009

#Plot Residuals
resids= (test.data$SpendPerVisit -exp(lasso.predicts))
plot(exp(lasso.predicts), resids)
plot(test.data$date, resids)
qqnorm(resids)
qqnorm(log(data$SpendPerVisit))
##################
#Ridge Regression#
##################


# Repeat the above for ridge regression
ridge.mod <- glmnet(train.matirx,log(train.data$SpendPerVisit), alpha =0, lambda = grid, thresh = 1e-12)

set.seed(1)
ridge.out <- cv.glmnet(train.matirx, log(train.data$SpendPerVisit), lambda = grid, alpha = 0)
plot(ridge.out)
bestlam2 <- ridge.out$lambda.min

ridge.predicts <- predict(ridge.mod, s = bestlam, newx = test.matrix)

mean((exp(ridge.predicts)-test.data$SpendPerVisit)^2) #5,716,153

coef(ridge.out, id = which.min(ridge.out$lambda))

#Calculate R^2

#R^2 = SSR/SST
SSR.ridge = sum((exp(ridge.predicts)- mean(train.data$SpendPerVisit))^2)
SST.ridge = sum((data$SpendPerVisit - mean(train.data$SpendPerVisit))^2)
R.squared.ridge = SSR.ridge/SST.ridge #0.008


