library(tree)
library(randomForest)
library(readr)
library(zoo)

#### Import the Data ####

data <- read_csv('final_dataset.csv')
data$SpendVisit <- data$new_spend/data$visits
data$SpendNight <- data$new_spend/data$nights
data$market <- as.factor(data$market)
data$duration <- factor(data$duration, levels=c("1-3  nights", "4-7  nights", "8-14 nights", "15+  nights"), ordered=TRUE)
data$method <- as.factor(data$method)
data$purpose <- as.factor(data$purpose)
data$date <- as.yearqtr(paste(data$Quarter, data$Year, sep = ' '), format = 'Q%q %Y')

data$Quarter <- factor(data$Quarter, levels = c("Q1", "Q2", "Q3", "Q4"), ordered = TRUE)




#### Regression Tree

tree1 <- tree(new_spend~.-date, data = data)
# This won't work - factor has at most 32 levels!

#### Pruned Regression Tree

#### Bagged Regression Tree
train <- which(data$Year != 2015)

trees <- c(25,100,250,500,1000)

set.seed(11182016)
spend.bagged <- randomForest(new_spend~.-date-SpendVisit-SpendNight,
                       data = data, subset = train , mtry = 9, importance = TRUE)

yhat.bag = predict(spend.bagged, newdata = data[-train,])
plot(yhat.bag, data$new_spend[-train])
abline(0,1)
mean((yhat.bag-data$new_spend[-train])^2)

bagged <- c()
bagged_models <- list()
set.seed(1101)
iter <- 0
for (tree in trees){
  iter <- iter + 1
  spend.bagged <- randomForest(new_spend~.-date-SpendVisit-SpendNight,
                               data = data, subset = train , mtry = 9, importance = TRUE,
                               ntree = tree)
  yhat.bag = predict(spend.bagged, newdata = data[-train,])
  bagged[iter] <- mean((yhat.bag-data$new_spend[-train])^2)
  bagged_models[[iter]] <- spend.bagged
}



#### Random Forest

spend.rf <- randomForest(new_spend~.-date-SpendVisit-SpendNight,
                            data = data, subset = train , mtry = 3, importance = TRUE)

yhat.rf <- predict(spend.rf, data[-train,])
mean((yhat.rf-data$new_spend[-train])^2)
#3.005

plot(spend.rf)

importance(spend.rf)
# Variable importance... first column is the mean decrease in accuracy in 
# predictions on out-of-bag sambles when said variable is EXCLUDED from the 
# model. The second colun measures the total decrease in node IMpurity resulting 
# from splits on the variable, averaged over all trees. Purity in Regression is
# RSS, classification uses deviance.

varImpPlot(spend.rf)
# LArger values of either indicate a greater importance.

trees <- c(5,10,25,50,100,250,500)
forest <- c()
forest_models <- list()
set.seed(1101)
iter <- 0
for (tree in trees){
  iter <- iter + 1
  spend.rf <- randomForest(new_spend~.-date-SpendVisit-SpendNight,
                               data = data, subset = train , mtry = 3, importance = TRUE,
                               ntree = tree)
  yhat.rf = predict(spend.rf, newdata = data[-train,])
  forest[iter] <- mean((yhat.rf-data$new_spend[-train])^2)
  forest_models[[iter]] <- spend.rf
}

# https://github.com/araastat/reprtree/blob/master/R/plot.getTree.R
source("TreeHelpers.R")

plot.getTree(spend.rf)


#### Time-Series

QTR_sum <- aggregate(data$new_spend, by = list(data$market, data$date), sum)
names(QTR_sum) <- c("market",'date','total_spend')
plot(QTR_sum$total_spend~QTR_sum$date, type='b')

library(ggplot2)

time <- ggplot(data = QTR_sum, aes(x=as.Date(date), y=total_spend))
time + geom_line(aes(color=market))


# Create plot of totals by method for a few markets
# Create a plot of totals for some markets.

