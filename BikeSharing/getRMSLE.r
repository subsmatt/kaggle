## Kaggle - Bike Sharing
## Sergey M. Sakantsev
## Calculate RMSLE - Root Mean Squared Logarithmic Error

## Args:
##      mod - model
##      t   - training set
##      c   - cv set
##      var - name of the dependant variable
getRMSLE = function(mod,t,c,var) {
    p = predict(mod)
    err.train = calcRMSLE(p, t[var], nrow(t)) 
    print(paste("RMSLE(", var, " ) Train: ", err.train))
    p1 = predict(mod, newdata=c)
    err.cv = calcRMSLE(p1, c[var], nrow(c))             
    print(paste("RMSLE(", var, ") CV: ", err.cv))  
}
