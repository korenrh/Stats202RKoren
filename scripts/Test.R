# standardize test data if needed
test_std <- standardize(test)

# verify test data
summary(test)
summary(test_std)

# predict
test_predict <- predict(svm, newdata=test_std)

# view prediction summary
summary(test_predict)

# make predictions data frame
predictions <- data.frame(
  id <- test$id,
  relevance <- predictions_gbm2 # insert predictions here
)

# write to CSV folder
write.csv(predictions, "./csv/predictions.csv", row.names = FALSE)

# General function to generate predictions
generate_predictions <- function(model, test_data) {
  test_predict <- predict(model, newdata=test_data)
  t <- read.csv("test.csv")
  predictions <- data.frame(
    id <- t$id,
    relevance <- test_predict
  )
  return(predictions)
}

# Ex: reading in poor performing GBM predictions
predboost <- read.csv("./csv/predboost.csv")
summary(predboost) # Predicting 79.81% relevance, that's a lot of false positives
