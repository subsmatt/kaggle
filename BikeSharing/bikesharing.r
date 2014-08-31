## Kaggle - Bike Sharing
## Sergey M. Sakantsev
## Model: Linear model

# Load the data
source("loaddata.r")
# Load the function to calculate RMSLE
source("calcRMSLE.r")

library(caTools)
set.seed(123)
split = sample.split(tmp$count, SplitRatio=0.7)
train = subset(tmp, split==TRUE)
cv = subset(tmp, split==FALSE)

#---------------------------------------
err.mean = mean(train$count)   #192.9493
#---------------------------------------

# Calculate RMSLE using err.mean on both training and cv sets
err.train.baseline = calcRMSLE(err.mean, train$count, nrow(train)) # RMSLE = 1.571293
err.cv.baseline = calcRMSLE(err.mean, cv$count, nrow(cv))          # RMSLE = 1.574484

# Linear Regression model
# mod.linrg = lm(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train) # R-squared = 0.4088
# mod.linrg = lm(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + mday + mon + wday, data=train) # R-squared = 0.3589
# mod.linrg = lm(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train) # R-squared = 0.4088
# mod.linrg = lm(count ~ holiday + weather + temp + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train) # R-squared = 0.4088
# mod.linrg = lm(count ~ weather + temp + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train) # R-squared = 0.4088
# mod.linrg = lm(count ~ temp + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train) # R-squared = 0.4068
# mod.linrg = lm(count ~ weather + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train) # R-squared = 0.4067
# mod.linrg = lm(count ~ weather + temp + humidity + windspeed + hour + mday + mon + year + wday, data=train) # R-squared = 0.4085
# mod.linrg = lm(count ~ weather + temp + windspeed + hour + mday + mon + year + wday, data=train) # R-squared = 0.4085

# Selected model------------------------
mod.linrg = lm(count ~ weather + temp + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train) # R-squared = 0.4088
#---------------------------------------
p = predict(mod.linrg)
p = abs(p)
err.train.linrg = calcRMSLE(p, train$count, nrow(train))   # RMSLE = 1.212041
p1 = predict(mod.linrg, newdata=cv)
p1 = abs(p1)
err.cv.linrg = calcRMSLE(p1, cv$count, nrow(cv))           # RMSLE = 1.232507
# Kaggle score = 1.26509
# Kaggle score (count values rounded up) = 1.26535


# CART Model
# install.packages("caret")
# install.packages("e1071")
# library(caret)
# library(e1071)

# Define cross-validation experiment
# Define how many folds
fitControl = trainControl(method="cv", number=10)
cartGrid = expand.grid(.cp=(1:50)*0.01)
train(count ~ weather + temp + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train, method="rpart", trControl=fitControl, tuneGrid=cartGrid)
# The final value used for the model was cp = 0.01.
mod.cart = rpart(count ~ weather + temp + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train, control=rpart.control(cp=0.01))
p = predict(mod.cart)
err.train.cart = calcRMSLE(p, train$count, nrow(train))   # RMSLE = 0.8605883
p1 = predict(mod.cart, newdata=cv)
err.cv.cart = calcRMSLE(p1, cv$count, nrow(cv))           # RMSLE = 0.8733523
# Kaggle score using mod.cart = 0.91705


# Random Forest
# install.packages("randomForest")
# library(randomForest)
mod.rf = randomForest(count ~ weather + temp + atemp + humidity + windspeed + hour + mday + mon + year + wday, data=train, nodesize=25, ntree=200)
p = predict(mod.rf)
err.train.rf = calcRMSLE(p, train$count, nrow(train))     # RMSLE = 0.6363678
p1 = predict(mod.rf, newdata=cv)
err.cv.rf = calcRMSLE(p1, cv$count, nrow(cv))             # RMSLE = 0.6367978
# for ntree=500
p = predict(mod.rf)
err.train.rf = calcRMSLE(p, train$count, nrow(train))     # RMSLE = 0.6543213
p1 = predict(mod.rf, newdata=cv)
err.cv.rf = calcRMSLE(p1, cv$count, nrow(cv))             # RMSLE = 0.6550684


