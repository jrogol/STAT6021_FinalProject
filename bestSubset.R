library(zoo)
library(leaps)

train <- read.csv('final_dataset.txt')

#convert factors
train$market <- as.factor(train$market)
train$duration <- factor(train$duration, levels=c("1-3  nights", "4-7  nights", "8-14 nights", "15+  nights"), ordered=TRUE)
train$method <- as.factor(train$method)
train$purpose <- as.factor(train$purpose)
train$Quarter <- factor(train$Quarter, levels = c('Q1','Q2','Q3','Q4'), ordered = TRUE)


#Best subset feature selection
############## Best Subset Selection ##############
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# Create a vector assigning each item to a fold:
set.seed(1)
k=10
folds <- sample(1:k, nrow(train), replace = TRUE)

#change this if we add more features
features <- 12

# matrix to store the results 12 variables wide, 10 folds tall
cv.errors.2 <- matrix(NA, k, features, dimnames = list(NULL, paste(1:features)))

# create a loop for CV!

for(j in 1:k){
  best.fit <- regsubsets(new_spend~., data = train[folds != j,], nvmax = features, really.big = T) # e/t but j as training!
  for (i in 1:features){
    pred <- predict(best.fit, train[folds==j,], id =i) # Predictions for j as test set, 1-43 times
    cv.errors.2[j,i] <- mean((train$new_spend[folds==j]-pred)^2) # Testing MSE!
  }
}

mean.cv.errors.2 <- apply(cv.errors.2,2, mean) # apply mean to the columns of errors for each column.
mean.cv.errors.2[which.min(mean.cv.errors.2)]
plot(mean.cv.errors.2, type = "b")

####CODE BELOW THIS LINE TAKES PROHIBITIVELY LONG####
reg.best2 <- regsubsets(new_spend~., data = train, nvmax = 50, really.big = TRUE)
coef(reg.best2, 5)
pred <- predict(reg.best2, train, 5)
best.mse2 <- mean((pred-train$new_spend)^2)
