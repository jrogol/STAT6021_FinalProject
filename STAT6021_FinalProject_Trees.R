library(tree)
library(randomForest)
library(readr)
library(zoo)
library(ggplot2)

#### Import the Data ####

data <- read_csv('raw_with_avg_added_final.csv')
# Visits is single unites, spend is single pouds, nights is in 1000's
data$market <- as.factor(data$market)
data$duration <- factor(data$duration, levels=c("1-3  nights", "4-7  nights", "8-14 nights", "15+  nights"), ordered=TRUE)
data$method <- as.factor(data$method)
data$purpose <- as.factor(data$purpose)
data$date <- as.yearqtr(paste(data$Quarter, data$Year, sep = ' '), format = 'Q%q %Y')

data$Quarter <- factor(data$Quarter, levels = c("Q1", "Q2", "Q3", "Q4"), ordered = TRUE)

str(data)
# 4 Quarters, 51 Markets, 4 durations, 3 methods, 5 purposes
# Year, Nights, Standard Rate (3)
# 70 total predictors, including dummy variables (8 total)


#### Transformation of Response ####

# Look at the distribuition of the data.

hist_pre <- ggplot(data = data, aes(x=SpendPerVisit)) 

pre_full <- hist_pre + 
  geom_histogram(aes(fill = ..count..)) +
  labs(title = 'Distribution of Response', x = 'Pounds Spend per Visit', y = 'Count')

pre_trimmed <- hist_pre + 
  geom_histogram(aes(fill = ..count..)) +
  xlim(0,4000) +
  labs(title = 'Distribution of Response', x = 'Pounds Spend per Visit (range limited)', y = 'Count')

# The data is heavily skewed - the response should be LOG(SpendPerVisit)
# As some of the data entries are 0, we add a minisule amount before proceeding.
data$AdjSPV <- data$SpendPerVisit + .000000000000001


hist_post <- ggplot(data = data, aes(x=log(AdjSPV)))

post_full <- hist_post +
  geom_histogram(aes(fill = ..count..)) +
  labs(title = 'Distribution of Transformed Response', x = 'Pounds Spend per Visit', y = 'Count')


post_trimmed <- hist_post +
  geom_histogram(aes(fill = ..count..)) +
  xlim(0,12) +
  labs(title = 'Distribution of Transformed Response', x = 'Pounds Spend per Visit (range limited)', y = 'Count')

# http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
# Use Cowplot to put ggplots on the same page!
library(cowplot)

plot_grid(pre_full, pre_trimmed, post_full, post_trimmed, ncol = 2, nrow =2)



#### Regression Tree ####

tree1 <- tree(log(AdjSPV)~.-SpendPerVisit-Currency-date-spend-visits, data = data)
# This won't work - the 'market' factor has more than 32 levels!

#### Bagged Regression Tree ####

# Create the testing data set's indices
train <- which(data$Year != 2015)

# Create a vector of the number of trees to grow
trees <- c(25,100,250,500,1000)

# Set Seed for reproducability
set.seed(11182016)

# Grow a bagged model.
spend.bagged <- randomForest(log(AdjSPV)~.-SpendPerVisit-Currency-date-spend-visits,
                       data = data, subset = train , mtry = 8, importance = TRUE)

# Predict with the held out data
yhat.bag = predict(spend.bagged, newdata = data[-train,])
# Test Set MSE
mean((exp(yhat.bag)-data$AdjSPV[-train])^2)

# Importance of the Predictors in the tree
importance(spend.bagged)

# Variable importance... first column is the mean decrease in accuracy in 
# predictions on out-of-bag sambles when said variable is EXCLUDED from the 
# model. The second colun measures the total decrease in node IMpurity resulting 
# from splits on the variable, averaged over all trees. Purity in Regression is
# RSS.

# Create empty vector to hold MSE's cross-validated by number of trees
bagged <- c()
# Create a list to store the models generated
bagged_models <- list()
# Set seed for reproducability
set.seed(1101)
# Set iteration variable
iter <- 0
# Loop through the 'trees' vector, growing bagged trees of various sizes
for (tree in trees){
  iter <- iter + 1
  spend.bagged <- randomForest(log(AdjSPV)~.-SpendPerVisit-Currency-date-spend-visits,
                               data = data, subset = train , mtry = 8, importance = TRUE,
                               ntree = tree)
  yhat.bag = predict(spend.bagged, newdata = data[-train,])
  bagged[iter] <- mean((exp(yhat.bag)-data$AdjSPV[-train])^2)
  bagged_models[[iter]] <- spend.bagged
}

# Model 6 has the lowest MSE
min(bagged)
bagged_models[[which.min(bagged)]]

y = data$AdjSPV[-train]
predicted = predict(bagged_models[[which.min(bagged)]], newdata = data[-train,])
rsq = 1 - sum((y-exp(predicted))^2)/sum((y-mean(y))^2)

importance(bagged_models[[which.min(bagged)]])



#### Random Forests #### 


# Grow a default random forest
spend.rf <- randomForest(log(AdjSPV)~.-SpendPerVisit-Currency-date-spend-visits,
                            data = data, subset = train , mtry = 3, importance = TRUE)

# Predict with the model, find the MSE
yhat.rf <- predict(spend.rf, data[-train,])
mean((exp(yhat.rf)-data$AdjSPV[-train])^2)

spend.rf$mse # OOB MSE.

# Training MSE vs. number of trees grown.
plot(spend.rf)

importance(spend.rf)

varImpPlot(spend.rf)
# Larger values of either indicate a greater importance.

# Create vector of trees to grow
trees <- c(5,10,25,50,100,250,500)

# Create a vector to hold the test-set MSEs, and a list to hold the models proper.
# Set the seed and iterate over the above vector of trees
forest <- c()
forest_models <- list()
set.seed(1101)
iter <- 0
for (tree in trees){
  iter <- iter + 1
  spend.rf <- randomForest(log(AdjSPV)~.-SpendPerVisit-Currency-date-spend-visits,
                               data = data, subset = train , mtry = 3, importance = TRUE,
                               ntree = tree)
  yhat.rf = predict(spend.rf, newdata = data[-train,])
  forest[iter] <- mean((exp(yhat.rf)-data$AdjSPV[-train])^2)
  forest_models[[iter]] <- spend.rf
  print(tree)
}

# Plot Test-set MSE over the number of trees
par(mfrow=c(1,1))
plot(forest~trees)

importance(forest_models[[7]])

which.min(forest) # Model 1! Five trees.

# Calculate R-squared and adjusted R-squared for the tree.
y = data$AdjSPV[-train]
predicted = predict(forest_models[[1]], newdata = data[-train,])
rsq = 1 - sum((y-exp(predicted))^2)/sum((y-mean(y))^2)

n = length(y)
p = 8
adj_rsq = 1 - (1-rsq)*(n-1)/(n-p-1)


# https://github.com/araastat/reprtree/blob/master/R/plot.getTree.R
source("TreeHelpers.R")

plot.getTree(spend.rf)


#### Time-Series ####

library(dygraphs)
library(dplyr)
library(tidyr)


ts_data <- data %>%
  group_by(market, date) %>%
  summarise(total_spend=sum(spend)) %>%
  spread(market, total_spend, fill =0) %>%
  arrange(date)


z <- zooreg(ts_data,
            start = as.yearqtr("2006-1"),
            frequency = 4)

dygraph(z[,-1]) %>%
  dyLegend(showZeroValues = FALSE)


# Summarize the Spend per Visit by Year and Quarter
total <- data %>%
  group_by(date) %>%
  summarise(total = sum(SpendPerVisit)*1000000/(sum(visits)*1000)) %>%
  arrange(date)
# Turn summarized data into a time series.
ts <- ts(total$total, start = c(2006,1), frequency = 4)


# Plot the decomposed time series and ACF/PACF plots
plot(decompose(ts))
par(mfrow=c(1,2))
acf(ts)
pacf(ts)
par(mfrow=c(1,1))


# Forecasting
library(tseries)
library(forecast)
adf.test(diff(ts,4), alternative="stationary", k=0)

fit <- arima(ts, c(1, 1, 1),seasonal = list(order = c(1, 1, 1), period = 4))


pred <- predict(fit, n.ahead = 16)
ts.plot(ts, pred$pred)

fit2 <- auto.arima(ts)
fcast <- forecast(fit2,36)
plot(fcast)

hw <- HoltWinters(ts)
p <- predict(hw, n.ahead = 36, prediction.interval = TRUE)
all <- cbind(ts, p)
plot(hw)

dygraph(all, "Total Spent per Visit in London") %>%
  dySeries("ts", label = "Actual") %>%
  dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")

