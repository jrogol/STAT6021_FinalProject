# The responses aren't Normally distributed.


mean(data$SpendPerVisit)
sd(data$SpendPerVisit)

temp <- data[data$SpendPerVisit < 8000,]
hist((data$SpendPerVisit)^(.25))
hist((temp$SpendPerVisit)^(.5))
