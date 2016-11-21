setwd("~/Downloads")
library(DAAG)
dataset <- read.csv('final_dataset')
names(dataset)

dataset$market <- as.factor(dataset$market)
dataset$duration <- factor(dataset$duration, levels=c("1-3  nights", "4-7  nights", "8-14 nights", "15+  nights"), ordered=TRUE)
dataset$method <- as.factor(dataset$method)
dataset$purpose <- as.factor(dataset$purpose)


s.null <- lm(new_spend~1, data=dataset)
s.full <- lm(new_spend~., data=dataset)

## Forward selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward") 
#new_spend ~ duration + purpose + nights + market + visits + Year + method + Quarter

## Backward selection
step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward")
#new_spend ~ Year + Quarter + market + duration + method + purpose + visits + nights

## Stepwise selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")
#new_spend ~ duration + purpose + nights + market + visits + Year + method + Quarter


#All gives the same model

#Perform 10-fold CV:

#Stepwise Selection CV:
stepwise.fit<- lm(new_spend~ duration +  purpose + nights + market + visits + Year + method + Quarter, data = dataset)
cv.lm(dataset, stepwise.fit, m = 10) #MSE = 1.95


