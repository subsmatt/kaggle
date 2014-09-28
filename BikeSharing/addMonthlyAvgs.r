## Kaggle - Bike Sharing
## Sergey M. Sakantsev
## Add montly averages
years = unique(tmp$year)
months = unique(tmp$mon)

for (y in 1:length(years)) {
  count.avg.list = tapply(tmp$count[tmp$year == years[y]], tmp$mon[tmp$year == years[y]], mean)
  
  for (m in 1:length(count.avg.list)){
    tmp$count_avg[tmp$year == years[y] & tmp$mon == m-1] = count.avg.list[[m]]    
    test$count_avg[test$year == years[y] & test$mon == m-1] = count.avg.list[[m]]
  }
}
