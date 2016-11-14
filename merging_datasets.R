country_currency <- read.csv('country_currency.csv', header = F, na.strings = c("", "NA"))

country_currency <- country_currency[,c(1,3)]
country_currency <- na.omit(country_currency)
names(country_currency) <- c('market', 'currency')

avgrates_csv <- read.csv('final_AVG.csv', header = F)
names(avgrates_csv) <- c('currency', 'year', 'quarter', 'quarterly average')

raw_csv <- read.csv('international-visitors-london-raw.csv', header = T)

raw_with_currency <- merge(x = raw_csv, y= country_currency, by = 'market')

#raw_with_currency <- merge(x = raw_csv, y= country_currency, by = 'market', all.x = TRUE)
#need_to_delete <- raw_with_currency[is.na(raw_with_currency$currency),] 
#11 countries without ex rates info --> 7183 observations without currency

#take out observations before 2006
raw_with_currency_updated <- raw_with_currency[raw_with_currency$year > 2005,]
raw_with_currency_and_avgRate <- merge(x = raw_with_currency_updated, y = avgrates_csv, by= c("currency", "year", 'quarter'), all.x = TRUE)

write.table(raw_with_currency_and_avgRate, file="raw_with_avg_added.csv", row.names=F, col.names=F, sep=",")


