## Kaggle - Bike Sharing
## Sergey M. Sakantsev
## Setup the environment

# define common variales
gYEAR          = 2011
gMON           = 0
gCHUNK         = 1
gRunSetup      = 1
gIncludeCasual = 0
gRunIterative  = 0

print(">> Bike Sharing Exercise Start")

# Load R packages and functions
if (gRunSetup == 1) {
  print("Setting up the environment...")
  source("setup.r")
  print("Setting up is complete.")

#  print("Training the base RF model...")
#  # Tune RF on the whole training set
#  findmtry <- tuneRF(cbind(train$season, train$holiday, train$workingday, train$weather, train$atemp, train$dtemp, train$humidity, train$windspeed, train$hour, train$mon, train$year, train$wday, train$count_avg), train$count, ntreeTry=100, stepFactor=1.5,improve=0.01, trace=FALSE, plot=FALSE, dobest=FALSE)

  # get the value of mtry with min OOBError
#  bestmtry = findmtry[findmtry[,2] == min(findmtry[,2])][1]
  
#  # Train RF on the whole training set
#  mod.rf = randomForest(count ~ season + holiday + workingday + weather + atemp + dtemp + humidity + windspeed + hour + mon + year + wday + count_avg, data=train, ntree=200, nodesize=5, mtry=bestmtry)

#  # Results for the RF model trained on the whole training set
#  getRMSLE(mod.rf, train, cv, "count")
#  print("====================================")
  # RMSLE.train  = 0.3321695
  # RMSLE.cv     = 0.3363893
}

train$count_est = 0
cv$count_est    = 0
test$count_est  = 0

if (gRunIterative == 1) {
  # Iterate over the given time period month by month (each month's obeservations represent a chunk)
  print("Training the iterative RF model...")
  for (y in length(years)){
    gYEAR = years[y]
    for (m in 1:length(months)){
      gMON = months[m]
      train.chunk = train[train$year <= gYEAR & train$mon <= gMON,]
      cv.chunk    = cv[cv$year <= gYEAR & cv$mon <= gMON,]
      test.chunk  = test[test$year <= gYEAR & test$mon <= gMON,]

      print("")
      print(paste("Chunk[", gCHUNK, "]:", gYEAR, "|", gMON, " Size:", nrow(train.chunk), "|", nrow(cv.chunk)))
      print("------------------------------------")

      # Get the estimate of 'Registered'
      #===================================================
      findmtry.chunk.registered <- tuneRF(cbind(train.chunk$season, train.chunk$holiday, train.chunk$workingday, train.chunk$weather, train.chunk$atemp,   train.chunk$dtemp, train.chunk$humidity, train.chunk$windspeed, train.chunk$hour, train.chunk$mon, train.chunk$year, train.chunk$wday, train.chunk$count_avg), train.chunk$registered, ntreeTry=200, stepFactor=1.5,improve=0.01, trace=FALSE, plot=FALSE, dobest=FALSE);

      bestmtry.chunk.registered = findmtry.chunk.registered[findmtry.chunk.registered[,2] == min(findmtry.chunk.registered[,2])][1]
    
      mod.rf.chunk.registered = randomForest(registered ~ season + holiday + workingday + weather + atemp + dtemp + humidity + windspeed + hour + mon + year + wday + count_avg, data=train.chunk, ntree=200, nodesize=5, mtry=bestmtry.chunk.registered)

      pred.rf.train.chunk.registered = predict(mod.rf.chunk.registered, newdata=train.chunk)
      train.chunk$registered_est = pred.rf.train.chunk.registered
      pred.rf.cv.chunk.registered = predict(mod.rf.chunk.registered, newdata=cv.chunk)
      cv.chunk$registered_est = pred.rf.cv.chunk.registered
      pred.rf.test.chunk.registered = predict(mod.rf.chunk.registered, newdata=test.chunk)
      test.chunk$registered_est = pred.rf.test.chunk.registered

      #getEstRMSLE(train.chunk, cv.chunk, "registered_est", "registered")


      # Use the esitmate of 'Casual' to predict 'Count'
      #===================================================
      if (gIncludeCasual == 1){
        findmtry.chunk.casual <- tuneRF(cbind(train.chunk$season, train.chunk$holiday, train.chunk$workingday, train.chunk$weather, train.chunk$atemp, train.chunk$dtemp, train.chunk$humidity, train.chunk$windspeed, train.chunk$hour, train.chunk$mon, train.chunk$year, train.chunk$wday, train.chunk$count_avg, train.chunk$registered_est), train.chunk$casual, ntreeTry=200, stepFactor=1.5,improve=0.01, trace=FALSE, plot=FALSE, dobest=FALSE);

        bestmtry.chunk.casual = findmtry.chunk.casual[findmtry.chunk.casual[,2] == min(findmtry.chunk.casual[,2])][1]
        #print(paste("Best mtry on current chunk:", bestmtry.chunk))

        mod.rf.chunk.casual = randomForest(casual ~ season + holiday + workingday + weather + atemp + dtemp + humidity + windspeed + hour + mon + year + wday + count_avg + registered_est, data=train.chunk, ntree=200, nodesize=5, mtry=bestmtry.chunk.casual)

        pred.rf.train.chunk.casual = predict(mod.rf.chunk.casual, newdata=train.chunk)
        train.chunk$casual_est = pred.rf.train.chunk.casual
        pred.rf.cv.chunk.casual = predict(mod.rf.chunk.casual, newdata=cv.chunk)
        cv.chunk$casual_est = pred.rf.cv.chunk.casual
        pred.rf.test.chunk.casual = predict(mod.rf.chunk.casual, newdata=test.chunk)
        test.chunk$casual_est = pred.rf.test.chunk.casual
      } else {
        train.chunk$casual_est = 0
        cv.chunk$casual_est    = 0
        test.chunk$casual_est  = 0
      }

      # Use the esitmate of 'Registered' to predict 'Count'
      #===================================================
      findmtry.chunk <- tuneRF(cbind(train.chunk$season, train.chunk$holiday, train.chunk$workingday, train.chunk$weather, train.chunk$atemp, train.chunk$dtemp, train.chunk$humidity, train.chunk$windspeed, train.chunk$hour, train.chunk$mon, train.chunk$year, train.chunk$wday, train.chunk$count_avg, train.chunk$registered_est, train.chunk$casual_est), train.chunk$count, ntreeTry=200, stepFactor=1.5,improve=0.01, trace=FALSE, plot=FALSE, dobest=FALSE);

      bestmtry.chunk = findmtry.chunk[findmtry.chunk[,2] == min(findmtry.chunk[,2])][1]
      #print(paste("Best mtry on current chunk:", bestmtry.chunk))

      mod.rf.chunk = randomForest(count ~ season + holiday + workingday + weather + atemp + dtemp + humidity + windspeed + hour + mon + year + wday + count_avg + registered_est + casual_est, data=train.chunk, ntree=200, nodesize=5, mtry=bestmtry.chunk)

      pred.rf.train.chunk = predict(mod.rf.chunk, newdata=train.chunk)
      train.chunk$count_est = pred.rf.train.chunk
      train$count_est[train$year == gYEAR & train$mon == gMON] = train.chunk$count_est[train.chunk$year == gYEAR & train.chunk$mon == gMON]
      pred.rf.cv.chunk = predict(mod.rf.chunk, newdata=cv.chunk)
      cv.chunk$count_est = pred.rf.cv.chunk
      cv$count_est[cv$year == gYEAR & cv$mon == gMON] = cv.chunk$count_est[cv.chunk$year == gYEAR & cv.chunk$mon == gMON]
      pred.rf.test.chunk = predict(mod.rf.chunk, newdata=test.chunk)
      test.chunk$count_est = pred.rf.test.chunk
      test$count_est[test$year == gYEAR & test$mon == gMON] = test.chunk$count_est[test.chunk$year == gYEAR & test.chunk$mon == gMON]

      # Compare the error estimate
      # Error estimate using iterative model
      getRMSLE(mod.rf.chunk, train.chunk, cv.chunk, "count")
      # Error estimate using the model trained on the whole training set
      getRMSLE(mod.rf, train.chunk, cv.chunk, "count")

      getEstRMSLE(train, cv, "count_est", "count")
      gCHUNK = gCHUNK + 1
    }
  }
} else {
  # Do not use iterative approach
  print("Training RF model...")
  # Tune RF on the whole training set
  #  findmtry <- tuneRF(cbind(train$season, train$holiday, train$workingday, train$weather, train$atemp, train$humidity, train$windspeed, train$hour, train$mon, train$year, train$wday, train$count_avg), train$registered, ntreeTry=200, stepFactor=1.5,improve=0.01, trace=FALSE, plot=FALSE, dobest=FALSE)

  # get the value of mtry with min OOBError
  #  bestmtry = findmtry[findmtry[,2] == min(findmtry[,2])][1]
  #  print(paste("BestMTRY =", bestmtry))

  mod.rf.reg = randomForest(registered ~ season + holiday + workingday + weather + atemp + humidity + windspeed + hour + mon + year + wday, data=train, ntree=500, mtry=9)

  train$registered_est = predict(mod.rf.reg)
  cv$registered_est = predict(mod.rf.reg, newdata=cv)
  test$registered_est = predict(mod.rf.reg, newdata=test)

  getEstRMSLE(train, cv, "registered_est", "registered")

  mod.rf = randomForest(count ~ season + holiday + workingday + weather + atemp + dtemp + humidity + windspeed + hour + mon + year + wday + registered_est, data=train, ntree=500, mtry=9)

  train$count_est = predict(mod.rf)
  cv$count_est = predict(mod.rf, newdata=cv)
  test$count_est = predict(mod.rf, newdata=test)

  getEstRMSLE(train, cv, "count_est", "count")
}

res <- test["datetime"]
res["count"] <- test$count_est
write.csv(res,"bikesharing_rf_results.csv", row.names=FALSE)

print("<< Bike Sharing Exercise End")
