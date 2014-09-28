## Kaggle - Bike Sharing
## Sergey M. Sakantsev
## Load the training and the test data

# read training data
tmp = read.csv("train.csv")
# get rid off unwanted columns
#tmp = tmp[c(1:9, 12)]

# convert datatime to a date datatype
tmp$datetime <- as.POSIXlt(tmp$datetime, "%Y-%m-%d %H:%M:%S")
# create new variables
tmp["hour"]  <- tmp$datetime$hour
tmp["mday"]  <- tmp$datetime$mday
tmp["mon"]   <- tmp$datetime$mon
tmp["year"]  <- tmp$datetime$year
tmp["year"]  <- tmp$year + 1900
tmp["wday"]  <- tmp$datetime$wday
tmp["yday"]  <- tmp$datetime$yday
tmp["dtemp"] <- tmp$temp - tmp$atemp
tmp["registered_est"] <- 0
tmp["casual_est"] <- 0
tmp["count_est"]  <- 0
tmp["count_avg"]  <- 0
# Convert season, weather, workingday, etc. to factor variables
tmp$season     <- as.factor(tmp$season)
tmp$holiday    <- as.factor(tmp$holiday)
tmp$workingday <- as.factor(tmp$workingday)
tmp$weather    <- as.factor(tmp$weather)
tmp$wday       <- as.factor(tmp$wday)
tmp$mon        <- as.factor(tmp$mon)

# read testing data
test <- read.csv("test.csv")

# convert datatime to a date datatype
test$datetime <- as.POSIXlt(test$datetime, "%Y-%m-%d %H:%M:%S")
# create new variables
test["hour"]  <- test$datetime$hour
test["mday"]  <- test$datetime$mday
test["mon"]   <- test$datetime$mon
test["year"]  <- test$datetime$year
test["year"]  <- test$year + 1900
test["wday"]  <- test$datetime$wday
test["yday"]  <- test$datetime$yday
test["dtemp"] <- test$temp - test$atemp
test["registered_est"] <- 0
test["casual_est"] <- 0
test["count_est"]  <- 0
test["count_avg"]  <- 0
# Convert season, weather, workingday, etc. to factor variables
test$season     <- as.factor(test$season)
test$holiday    <- as.factor(test$holiday)
test$workingday <- as.factor(test$workingday)
test$weather    <- as.factor(test$weather)
test$wday       <- as.factor(test$wday)
test$mon        <- as.factor(test$mon)
