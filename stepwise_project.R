setwd("~/Desktop/STAT6021_FinalProject-master-2")
library(DAAG)
dataset <- read.csv('raw_with_avg_added_final.csv')
names(dataset)
library(MASS)

library(tree)
library(randomForest)
library(readr)
library(zoo)

#dataset$SpendVisit <- dataset$new_spend/dataset$visits
#dataset$SpendNight <- dataset$new_spend/dataset$nights



dataset$market <- as.factor(dataset$market)
dataset$duration <- factor(dataset$duration, levels=c("1-3  nights", "4-7  nights", "8-14 nights", "15+  nights"), ordered=TRUE)
dataset$method <- as.factor(dataset$method)
dataset$purpose <- as.factor(dataset$purpose)
dataset$date <- as.yearqtr(paste(dataset$Quarter, dataset$Year, sep = ' '), format = 'Q%q %Y')

dataset$Quarter <- factor(dataset$Quarter, levels = c("Q1", "Q2", "Q3", "Q4"), ordered = TRUE)

dataset <- dataset[,-c(1,8,9,13)]


i=0
for (i in 1:nrow(dataset)){
  if (dataset$SpendPerVisit[i] == 0){
    dataset$SpendPerVisit[i] = 0.0000001
  }
}


mean(dataset$SpendPerVisit)



for (i in 1:nrow(dataset)){
  if (dataset$SpendPerVisit[i] > 500 ){
    dataset$greater500[i] = 1
  }else{
    dataset$greater500[i] = 0
  }
}


train = dataset[dataset$Year != 2015,]
test = dataset[dataset$Year == 2015,]

logistic <- glm(greater500 ~ Year + Quarter +market +duration + method+purpose+nights+StandardRate, data=train, family = 'binomial')


summary(logistic)


predicted <- predict(logistic, newdata = test, type = 'response')
predicted

round_pred <- round(predicted)


library(ROCR)

pred = prediction(predicted, test$greater500)
roc.curve<- performance(pred, 'tpr', 'fpr')
auc = performance(pred, "auc")@y.values[[1]]
auc # 0.8352237

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

# s.null <- lm(log(SpendPerVisit)~1, data=train)
# s.full <- lm(log(SpendPerVisit)~., data=train)
# 
# 
# 
# ## Forward selection
# step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward") 
# #log(SpendPerVisit) ~ method + market + duration + purpose + nights + Year + Quarter + StandardRate
# 
# ## Backward selection
# step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward")
# #log(SpendPerVisit) ~ Year + Quarter + market + duration + method + purpose + nights + StandardRate
# 
# ## Stepwise selection
# step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")
# #log(SpendPerVisit) ~ method + market + duration + purpose + nights + Year + Quarter + StandardRate
# 
# 
# stepwise.fit1 <- lm(log(SpendPerVisit) ~ method + market + duration + purpose + nights + Year + Quarter + StandardRate, data = train)
# summary(stepwise.fit1) #0.1549 
# 
# log_test_predicted1 <- predict(stepwise.fit1, newdata = test, type= 'response')
# 
# test_predicted <- exp(log_test_predicted1)
# MSE1 <- mean((test_predicted - test$SpendPerVisit)^2)
# MSE1 #5715789
# 
# plot(stepwise.fit, stepwise.fit1$residuals)
# 
# histogram(log(dataset$SpendPerVisit))
# 
