## Kaggle - Bike Sharing
## Sergey M. Sakantsev
## Calculate RMSLE - Root Mean Squared Logarithmic Error

## Args:
##      t      - training set
##      c      - cv set
##      varest - name of the variable that holds estimated values of the dependant variable
##      var    - name of the dependant variable
getEstRMSLE = function(t,c,varest,var) {
    err.train = calcRMSLE(t[varest], t[var], nrow(t)) 
    print(paste(">> Est RMSLE(", var, ") Train: ", err.train))
    err.cv = calcRMSLE(c[varest], c[var], nrow(c))     
    print(paste(">> Est RMSLE(", var, ") CV: ", err.cv))  
}
