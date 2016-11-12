library(dplyr)
ex.rates = read.csv('exrates.csv')

#sort by currency, then year
by_currency_year <- arrange(ex.rates, currency, year)


#split 2016 from 2015
rate_til2015 <- by_currency_year [by_currency_year $year != 2016,]
rates_2016 <- by_currency_year [by_currency_year $year == 2016,]

#sort by currency, then year, then month
#by_currency_year 2015 <- sort(rate_til2015, rate_til2015$currency, rate_til2015$year, rate_til2015$month)

#find average of every three row (valid because it is sorted)
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
#rates_2016 <- by_currency_year [by_currency_year $year == 2016,]
rates_2016_til_sep <- rates_2016[rates_2016$month != 'Oct' & rates_2016$month != 'Nov',]
rates_2016_rest <-rates_2016[rates_2016$month == 'Oct' | rates_2016$month == 'Nov',]

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

currency_list <- c()
for(i in 1:length(currency)){
  each_currency <- rep(currency[i],3)
  currency_list <- c(currency_list,each_currency)
}
currency_list 


year_list <- c()
for(i in 1:length(year)){
  each_year <- rep(year[i],3)
  year_list <- c(year_list,each_year)
}
year_list <- rep(year_list, 40)

quarter_4 <- c('Q1', 'Q2', 'Q3')
quarter_list <- rep(quarter_4, 40)
quarter_list

dataframe2016_tilsep <- cbind(currency_list,year_list, quarter_list, mean_ex_rate )


#repeat the above for the rest of 2016 
#rates_2016_rest <-rates_2016[rates_2016$month == 'Oct' | rates_2016$month == 'Nov',]

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






