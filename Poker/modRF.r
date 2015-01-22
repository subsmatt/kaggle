## Kaggle - Poker Rule Induction
## Sergey M. Sakantsev
## Model: Random Forests

# Variables
liTree = 200

# Use Random forests
print(paste('NTrees =', liTree))
mod.rf <- randomForest(factor(hand) ~., data=train, ntree=liTree, nodesize=10, mtry=9)
pred.rf <- predict(mod.rf)
print(paste('Train data accuracy:', sum(train$hand==pred.rf)/nrow(train)))
pred.rf <- predict(mod.rf, newdata=cv)
print(paste('CV data accuracy:', sum(cv$hand==pred.rf)/nrow(cv)))
pred.rf <- predict(mod.rf, newdata=test)
test$hand <- as.numeric(pred.rf)
