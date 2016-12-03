setwd("~/Desktop/STAT6021_FinalProject-master-2")
library(DAAG)
dataset <- read.csv('raw_with_avg_added_final.csv')
library(MASS)
library(tree)
library(randomForest)
library(readr)
library(zoo)
library(ROCR)


dataset$market <- as.factor(dataset$market)
dataset$duration <- factor(dataset$duration, levels=c("1-3  nights", "4-7  nights", "8-14 nights", "15+  nights"), ordered=TRUE)
dataset$method <- as.factor(dataset$method)
dataset$purpose <- as.factor(dataset$purpose)
dataset$Quarter <- factor(dataset$Quarter, levels = c("Q1", "Q2", "Q3", "Q4"), ordered = TRUE)

#take out currency, spend, and visit
dataset <- dataset[,-c(1,8,9)]

#split into training and testing
train = dataset[dataset$Year != 2015,]
test = dataset[dataset$Year == 2015,]


########################################
#     Non-transformed SpendPerVisit    #
########################################

s.null <- lm(SpendPerVisit~1, data=train)
s.full <- lm(SpendPerVisit~., data=train)

## Forward selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward")
#SpendPerVisit ~ duration + market + purpose + nights + Year + method + Quarter

## Backward selection
step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward")
#SpendPerVisit ~ Year + Quarter + market + duration + method + purpose + nights


## Stepwise selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")
#SpendPerVisit ~ duration + market + purpose + nights + Year + method + Quarter


model <- lm(SpendPerVisit ~ duration + market + purpose + nights + Year + method + Quarter, data=train)
summary(model)

test_predicted_forward_stepwise <- predict(model, test)
MSE_forward_stepwise <- mean((test_predicted_forward_stepwise - test$SpendPerVisit)^2)
MSE_forward_stepwise #5456789

plot(model$fitted.values, model$residuals)


################################################################################

########################################
#     Log-transformed SpendPerVisit    #
########################################

#log transformed only takes value greater than 0 
#so we changed all 0 values into values only a slightly greater than 0


for (i in 1:nrow(dataset)){
  if (dataset$SpendPerVisit[i] == 0){
    dataset$SpendPerVisit[i] = 0.0000001
  }
}

train = dataset[dataset$Year != 2015,]
test = dataset[dataset$Year == 2015,]


s.null <- lm(log(SpendPerVisit)~1, data=train)
s.full <- lm(log(SpendPerVisit)~., data=train)

## Forward selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward")
#log(SpendPerVisit) ~ method + market + duration + purpose + nights + Year + Quarter + StandardRate

## Backward selection
step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward")
# log(SpendPerVisit) ~ Year + Quarter + market + duration + method + purpose + nights + StandardRate

## Stepwise selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")
#log(SpendPerVisit) ~ method + market + duration + purpose + nights + Year + Quarter + StandardRate

log_stepwise <- lm(log(SpendPerVisit) ~ method + market + duration + purpose + nights + Year + Quarter + StandardRate, data = train)
summary(log_stepwise) #0.1549

log_test_predicted <- predict(log_stepwise, newdata = test, type= 'response')

test_predicted <- exp(log_test_predicted)
log_MSE <- mean((test_predicted - test$SpendPerVisit)^2)
log_MSE #5715789



################################################################################


#####################################################
# Binary Classification using Logistic Regression   #
#####################################################

#Create a column called greater500
#if SpendPerVisit is greater than 500, assign 1 to the corresponding 
#greater500 cell
#if less than 500, assign 0 to the corresponding greater500 cell

for (i in 1:nrow(dataset)){
  if (dataset$SpendPerVisit[i] > 500 ){
    dataset$greater500[i] = 1
  }else{
    dataset$greater500[i] = 0
  }
}


train = dataset[dataset$Year != 2015,]
test = dataset[dataset$Year == 2015,]

logistic <- glm(greater500 ~ Year + Quarter +market +duration + method + purpose+
                  nights+StandardRate, data=train, family = 'binomial')
summary(logistic)
predicted <- predict(logistic, newdata = test, type = 'response')
predicted

round_pred <- round(predicted)

pred = prediction(predicted, test$greater500)
roc.curve<- performance(pred, 'tpr', 'fpr')
auc = performance(pred, "auc")@y.values[[1]]
auc # 0.8352237

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

