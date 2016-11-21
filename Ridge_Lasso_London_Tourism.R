
library(glmnet)
data.lasso <- read.csv('final_dataset.csv')

#Drop Variables we don't need:

# Create a vector of lambdas to test, from very large to very small.
grid <- 10^seq(10, -10, length = 100)

# Turn the X variables into a matrix for ridge and lasso regression
x<- model.matrix(new_spend~., data = data.lasso)

# Create a lasso model on the response variable, spend
lasso.mod <- glmnet(x,data.lasso$new_spend, alpha =1, lambda = grid, thresh = 1e-12)

# Set the seed for reproducable results
set.seed(21)

# 10-fold cross-validated lasso regression
lasso.out <- cv.glmnet(x, data.lasso$new_spend, lambda = grid, alpha = 1)
plot(lasso.out)

# find the best lambda
bestlam <- lasso.out$lambda.min

# predict using the best lambda
lasso.predicts <- predict(lasso.mod, s = bestlam, newx = x)

#Take the Mean of the residuals  for the MSE
MSE<-mean((lasso.predicts-data.lasso$new_spend)^2) # 1.9369


#Coefficients of the model
coef(lasso.out, id = which.min(lasso.out$lambda))


#Ridge REgression

data.ridge<- read.csv('final_dataset.csv')
# Repeat the above for ridge regression
ridge.mod <- glmnet(x,data.ridge$new_spend, alpha =0, lambda = grid, thresh = 1e-12)

set.seed(1)
ridge.out <- cv.glmnet(x, data.ridge$new_spend, lambda = grid, alpha = 0)
plot(ridge.out)
bestlam2 <- ridge.out$lambda.min

ridge.predicts <- predict(ridge.mod, s = bestlam, newx = x)

mean((ridge.predicts-data.ridge$new_spend)^2) # 1.9369

coef(ridge.out, id = which.min(ridge.out$lambda))

#Mean of New_spend
mean(data.lasso$new_spend) #0.6298706
