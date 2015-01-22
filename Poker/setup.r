## Kaggle - Poker Rule Induction
## Sergey M. Sakantsev
## Setup the environment

# Load R packages and functions
library(caTools)
library(randomForest)
source("loaddata.r")

# Set random seed
set.seed(118)

# Define variables
liSplitRatio = 0.7

split = sample.split(tmp$hand, SplitRatio=liSplitRatio)
train = subset(tmp, split==TRUE)
cv = subset(tmp, split==FALSE)
test$hand = ''

print(paste('Taining set:', nrow(train), 'rows.'))
print(paste('Cross validation set:', nrow(cv), 'rows.'))
print(paste('Test set:', nrow(test), 'rows.'))
