## Kaggle - Bike Sharing
## Sergey M. Sakantsev
## Setup the environment

# define common variales
gTRAIN_THRESHOLD = 15

# Load R packages and functions
library(caTools)
library(randomForest)
source("loaddata.r")
source("calcRMSLE.r")
#source("getRes.r")
source("getRMSLE.r")
source("getEstRMSLE.r")
source("addMonthlyAvgs.r")

# Set random seed
set.seed(118)
train = tmp[tmp$mday > 0 & tmp$mday < gTRAIN_THRESHOLD, ]
cv = tmp[tmp$mday >= gTRAIN_THRESHOLD, ]


