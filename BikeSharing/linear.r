## Kaggle - Bike Sharing
## Sergey M. Sakantsev
## Model: Linear model

# read training data
tmp = read.csv("train.csv")
# get rid off unwanted columns
tmp = tmp[c(1:9, 12)]

# convert datatime to a date datatype
tmp$datetime <- as.POSIXlt(tmp$datetime, "%Y-%m-%d %H:%M:%S")
# create new variables
tmp["hour"] <- tmp$datetime$hour
tmp["mday"] <- tmp$datetime$mday
tmp["mon"]  <- tmp$datetime$mon
tmp["year"] <- tmp$datetime$year
tmp["year"] <- tmp$year + 1900
tmp["wday"] <- tmp$datetime$wday
tmp["yday"] <- tmp$datetime$yday

# Convert season, weather, workingday, etc. to factor variables
tmp$holiday    <- as.factor(tmp$holiday)
tmp$workingday <- as.factor(tmp$workingday)
tmp$weather    <- as.factor(tmp$weather)
tmp$wday       <- as.factor(tmp$wday)
tmp$mon        <- as.factor(tmp$mon)

# Fit linear regression model
model <- lm(count ~ temp, data=tmp)                                                                   # R-squared = 0.1556
model <- lm(count ~ temp + humidity, data=tmp)                                                        # R-squared = 0.2411
model <- lm(count ~ temp + humidity + windspeed, data=tmp)                                            # R-squared = 0.2413
model <- lm(count ~ temp + humidity + hour, data=tmp)                                                 # R-squared = 0.3183
model <- lm(count ~ temp + humidity + hour + weather, data=tmp)                                       # R-squared = 0.3217
model <- lm(count ~ temp + humidity + hour + season + weather, data=tmp)                              # R-squared = 0.3461
model <- lm(count ~ temp + humidity + hour + season + weather + holiday, data=tmp)                    # R-squared = 0.3462
model <- lm(count ~ temp + humidity + hour + season + weather + holiday + workingday, data=tmp)       # R-squared = 0.3462
model <- lm(count ~ temp + humidity + hour + season + weather + workingday, data=tmp)                 # R-squared = 0.3461
model <- lm(count ~ temp + humidity + hour + season + weather + workingday + wday, data=tmp)          # R-squared = 0.3469
model <- lm(count ~ temp + humidity + hour + season + weather + workingday + wday + mon, data=tmp)    # R-squared = 0.3573

#the best model so far is:
model <- lm(count ~ temp + humidity + hour + season + weather + workingday + wday + mon, data=tmp)    # R-squared = 0.3573

# Get an error estimate for a baseline model (model that simply predicts the average value)
# Mean = 144.2233
RMSLE <- sqrt((sum((log(mean(tmp$count) + 1) + log(tmp$count))^2))/nrow(tmp))                         # RMSLE = 9.646442

model <- lm(count ~ temp + humidity + hour + season + weather + workingday + wday + mon, data=tmp)
# model$xlevels$weather <- union(model$xlevels$weather, levels(test$weather))

# Get an error estimate on the training data
p <- predict(model, newdata=train)
p <- abs(p)
RMSLE <- sqrt((sum((log(p + 1) + log(tmp$count))^2))/nrow(tmp))                                       # RMSLE = 9.817421

# Run the model on the test data
p1 <- predict(model, newdata=test)

# Write results to a csv file
p1 <- abs(p1)
res <- test["datetime"]
res["count"] <- p1
res$count <- round(res$count)
write.csv(res,"linear.csv", row.names=FALSE)
