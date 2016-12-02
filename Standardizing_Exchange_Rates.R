#Standardizing Exchange Rates
library(dplyr)
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

for(i in 1: length(standard)) {
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



write.table(raw_with_currency_and_st.avgRate, file="raw_with_avg_added_final.csv", row.names=FALSE, col.names=TRUE, sep=",")
