#Stat 6021 Final Project
#James Rogol, Tim Schroeder, Megan Stiles and Julina Zhang

library(dplyr)
library(tree)
library(randomForest)
library(readr)
library(zoo)
library(ggplot2)
library(glmnet)
library(readr)
library(zoo)
library(MASS)
library(DataCombine)
library(DAAG)
library(ROCR)
#######################################
######### Data Cleaning   #############
#######################################

#Find which countries are in data set to find exchange rates

#read in London Tourism Data
data <- read.csv('international-visitors-london-raw.csv')

#find Unique country names
unique_countries <- unique(data$market)

#Write out file of unique country names
write.table(unique_countries, file="countries.csv", row.names=F, col.names=F, sep=",")

#Finding the Average Exchange Rate for Each Quarter in order to standardize them

#Read in Data, Scraped from web using Python 
ex.rates = read.csv('ExchangeRates.csv')

#sort by currency, then year
by_currency_year <- arrange(ex.rates, currency, year)


#split 2016 from 2015
rate_til2015 <- by_currency_year [by_currency_year $year != 2016,]
rates_2016 <- by_currency_year [by_currency_year $year == 2016,]

#find average of every three rows (valid because it is sorted)
split <- seq(1, 4800, 3)
mean_ex_rate <- c()

for(i in 1:length(split)){
  quarterly_avg <- ((rate_til2015$rate[split[i]] +  rate_til2015$rate[split[i]+1]+ rate_til2015$rate[split[i]+2])/3)
  mean_ex_rate <- c(mean_ex_rate, quarterly_avg)
}
mean_ex_rate 


length((mean_ex_rate)) #1600
year<- unique(rate_til2015$year)
currency <- as.character(rate_til2015$currency)
currency <- unique(currency)
currency 

#getting currency, quarter, year column ready to bind with the mean exchange rate

currency_list <- c()
for(i in 1:length(currency)){
  each_currency <- rep(currency[i],4*10)
  currency_list <- c(currency_list,each_currency)
}
currency_list 


year_list <- c()
for(i in 1:length(year)){
  each_year <- rep(year[i],4)
  year_list <- c(year_list,each_year)
}
year_list <- rep(year_list, 40)

quarter_4 <- c('Q1', 'Q2', 'Q3', 'Q4')
quarter_list <- rep(quarter_4, 400)
quarter_list

#bind the four the lists
dataframe2015 <- cbind(currency_list,year_list, quarter_list, mean_ex_rate )


#repeat the above for 2016 up til Sep (because we havent gotten to December yet)
rates_2016_til_sep <- rates_2016[rates_2016$month != 'Oct' & rates_2016$month != 'Nov',]
rates_2016_rest <-rates_2016[rates_2016$month == 'Oct' | rates_2016$month == 'Nov',]

#Find Average Exchange Rate for Every 3 months (each quarter)
split <- seq(1, 360, 3)

mean_ex_rate <- c()

for(i in 1:length(split)){
  quarterly_avg <- ((rates_2016_til_sep$rate[split[i]] + rates_2016_til_sep$rate[split[i]+1]+ rates_2016_til_sep$rate[split[i]+2])/3)
  mean_ex_rate <- c(mean_ex_rate, quarterly_avg)
}
mean_ex_rate 


length(mean_ex_rate) #120
year<- unique(rates_2016_til_sep$year)
currency <- as.character(rates_2016_til_sep$currency)
currency <- unique(currency)
currency 

#Repeat Currency Type 3 times
currency_list <- c()
for(i in 1:length(currency)){
  each_currency <- rep(currency[i],3)
  currency_list <- c(currency_list,each_currency)
}
currency_list 

#Repeat Year 3 times
year_list <- c()
for(i in 1:length(year)){
  each_year <- rep(year[i],3)
  year_list <- c(year_list,each_year)
}
year_list <- rep(year_list, 40)

quarter_4 <- c('Q1', 'Q2', 'Q3')
quarter_list <- rep(quarter_4, 40)
quarter_list

#Bind currency list, year list, quarter list, and mean exchange rate together
dataframe2016_tilsep <- cbind(currency_list,year_list, quarter_list, mean_ex_rate )


#repeat the above for the rest of 2016 

split <- seq(1, 80, 2)

mean_ex_rate <- c()

for(i in 1:length(split)){
  quarterly_avg <- ((rates_2016_rest$rate[split[i]] +  rates_2016_rest$rate[split[i]+1])/2)
  mean_ex_rate <- c(mean_ex_rate, quarterly_avg)
}
mean_ex_rate 


length(mean_ex_rate) #40
year<- unique(rates_2016_rest$year)
currency <- as.character(rates_2016_rest$currency)
currency <- unique(currency)
currency 

currency_list <- c()
for(i in 1:length(currency)){
  each_currency <- rep(currency[i],1)
  currency_list <- c(currency_list,each_currency)
}
currency_list 


year_list <- c()
for(i in 1:length(year)){
  each_year <- rep(year[i],1)
  year_list <- c(year_list,each_year)
}
year_list <- rep(year_list, 40)

quarter_4 <- c('Q4')
quarter_list <- rep(quarter_4, 40)
quarter_list

dataframe2016_rest <- cbind(currency_list,year_list, quarter_list, mean_ex_rate )



#get final dataframe
final_dataframe <- rbind(dataframe2015, dataframe2016_tilsep, dataframe2016_rest)

write.table(final_dataframe, file="final_AVG.csv", row.names=F, col.names=F, sep=",")

########################################
###Standardizing Exchange Rates#########
########################################

#Standardizing Exchange Rates and data cleaning
standard<- read.csv('final_AVG.csv', header = FALSE)

#Rename Columns
col_names<- c('Currency', 'Year', 'Quarter', 'Rate')
names(standard)<- col_names

#Group Data set by Currency

by_currency<- group_by(standard, Currency)

#Find Mean By currency

currency_mean<- summarise(by_currency, meanCurrency = mean(Rate))
currency_mean

#Find SD By Currencu

currency_SD<- summarise(by_currency, sdCurrency = sd(Rate))
currency_SD

#Merge Mean and SD into Datafram for each observation
standard<- merge(standard, currency_mean)

standard<- merge(standard, currency_SD)

#Standardize Rates by subtracting mean and dividing by SD for each currency    
i=0

for(i in 1: nrow(standard)) {
  standard$StandardRate[i]<- ((standard$Rate[i]-standard$meanCurrency[i])/standard$sdCurrency[i])
}

#Write out Data Frame to csv

write.csv(standard, file = 'StandardizedRates.csv')


#Merge Standard Rates with Rest of Data

raw <- read.csv('raw_with_avg_added.csv', header= FALSE)
names(raw) <- c('Currency', 'Year', 'Quarter', 'market', 'duration', 'method', 'purpose', 'destination',
                'visits', 'spend', 'nights', 'sample', 'Rate')

standardized <- read.csv('StandardizedRates.csv')
raw_with_currency_and_st.avgRate <- merge(x = raw, y = standardized[,-1], by= c("Currency", "Year", 'Quarter', 'Rate'))


#Write out data set with standardized rates
write.table(raw_with_currency_and_st.avgRate, file="raw_with_avg_added_final.csv", row.names=FALSE, col.names=TRUE, sep=",")


#Create new Response Variable: SpendPerVisit
#Read in clean data set
complete_data<- read.csv('raw_with_avg_added_final.csv')

#Drop unnecessary predictors
complete_data <- complete_data[, -c(4,9,13,14,15)]


#Rescale Spend Variable
complete_data$spend<-complete_data$spend*1000000

#Rescale Visit Variable
complete_data$visits<- complete_data$visits*1000

#Create new SpendPerVisit Variable
complete_data$SpendPerVisit<- (complete_data$spend/complete_data$visits)

#Write out clean Data Set

write.table(complete_data, file="raw_with_avg_added_final.csv", row.names=FALSE, col.names=TRUE, sep=",")

######################################
######Data Analysis###################
######################################

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

######################################################################
####### Model Selection with Transformed Response Variable############
#####################################################################
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

#########################################
###### Stepwise Model Selection##########
#########################################

##Read in Data
dataset <- read.csv('raw_with_avg_added_final.csv')
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

#Split Data into Training and Testing
train = dataset[dataset$Year != 2015,]
test = dataset[dataset$Year == 2015,]

#Perform Logistic Regression
logistic <- glm(greater500 ~ Year + Quarter +market +duration + method + purpose+
                  nights+StandardRate, data=train, family = 'binomial')
summary(logistic)
predicted <- predict(logistic, newdata = test, type = 'response')
predicted

round_pred <- round(predicted)

#Plot ROC Curve and Calculate AUC
pred = prediction(predicted, test$greater500)
roc.curve<- performance(pred, 'tpr', 'fpr')
auc = performance(pred, "auc")@y.values[[1]]
auc # 0.8352237

# plot
plot(roc.curve, main = paste("ROC (AUC=", round(auc,2), ")", sep=""))
abline(0, 1, lty="dashed")

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