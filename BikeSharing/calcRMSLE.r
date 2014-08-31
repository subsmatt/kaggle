## Kaggle - Bike Sharing
## Sergey M. Sakantsev
## Calculate RMSLE - Root Mean Squared Logarithmic Error

## Args:
##      p - predicted values
##      a - actual values
##      n - number of entries

calcRMSLE = function(p,a,n) {
    RMSLE = sqrt((sum((log(p + 1) - log(a + 1))^2))/n)
}
