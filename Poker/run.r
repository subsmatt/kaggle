## Kaggle - Poker Rule Induction
## Sergey M. Sakantsev
## Run the model

# define common variales
if (!exists('gbSetup')) {
  gbSetup = FALSE
}

# setup the environment
if (!gbSetup) {
  gbSetup = TRUE
  print("Setting up the environment...")
  source('setup.r')
}

# Use Random forests
print("Running the model...")
source('modRF.r')

res <- test[c('id','hand')]
write.csv(res,"poker_rf_results.csv", row.names=FALSE)
