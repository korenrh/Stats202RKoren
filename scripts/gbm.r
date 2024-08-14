install.packages("gbm")
library(gbm)
install.packages("caret")
library(caret)

training <- read.csv("training.csv")
test <- read.csv("test.csv")
training$is_homepage <- as.factor(training$is_homepage)
test$is_homepage <- as.factor(test$is_homepage)
training$relevance <- as.factor(training$relevance)
training <- training[, 3:13]
test <- test[, 3:12]

# tuning parameter grid
tune_grid <- expand.grid(
  n.trees = c(100, 200, 300),
  interaction.depth = c(2, 4, 6),
  shrinkage = c(0.025, 0.05, 0.075, 0.1),
  n.minobsinnode = c(20)
)

# set up 5-fold CV
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# use grid to try different combinations
gbm <- train(
  relevance ~ .,
  data = training,
  method = "gbm",
  trControl = train_control,
  tuneGrid = tune_grid,
  verbose = FALSE
)

# print best parameters
print(gbm$bestTune) #200, 6, 0.075

# get results
results <- gbm$results
sorted_results <- results[order(-results$Accuracy), ]
print(sorted_results)

ggplot(results, aes(x = interaction.depth, y = n.trees, fill = Accuracy)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Accuracy Heatmap",
       x = "Depth",
       y = "# of Trees",
       fill = "Accuracy") +
  facet_wrap(~ shrinkage) +
  theme_minimal()

# convert relevance to numeric to fit
training_test <- training
training_test$relevance <- as.numeric(as.factor(training$relevance)) - 1

# fit on full dataset
gbm_try <- gbm(
  relevance ~ .,
  data = training_test,
  distribution = "bernoulli",
  n.trees = 200,
  interaction.depth = 6,
  shrinkage = 0.075,
  n.minobsinnode = 20
)

# make predictions
predict_gbm <- predict(gbm_try, newdata=test, n.trees=300, type="response")
predictions_gbm <- ifelse(predict_gbm > 0.5, 1, 0)
summary(predictions_gbm)

# -----------------------------------------------------------------------

# tuning parameter grid
tune_grid <- expand.grid(
  n.trees = c(300, 400, 500),
  interaction.depth = c(4,6,8),
  shrinkage = c(0.02, 0.04, 0.6, 0.08, 0.1, 0.12, 0.14),
  n.minobsinnode = c(20)
)

# setup 5-fold CV
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# use grid to try different combinations
gbm2<- train(
  relevance ~ .,
  data = training,
  method = "gbm",
  trControl = train_control,
  tuneGrid = tune_grid,
  verbose = FALSE
)

# print best parameters
print(gbm2$bestTune) #400, 4, 0.08
results2 <- gbm2$results

sorted_results2 <- results2[order(-results2$Accuracy), ]
print(sorted_results2) # Best: 0.6675

# ---- practice predicting on a random training/test split

n <- nrow(training)
train_size <- floor(n * 0.7)

train_i <- sample(seq_len(n), size = train_size)
train_gbm <- training[train_i, ]
test_gbm <- training[-train_i, ]

# bernoulli needs numeric
train_gbm$relevance <- as.numeric(as.factor(train_gbm$relevance)) - 1

gbm_try2 <- gbm(
  relevance ~ .,
  data = train_gbm,
  distribution = "bernoulli",
  n.trees = 400,
  interaction.depth = 4,
  shrinkage = 0.08,
  n.minobsinnode = 20
)

predict_gbm2 <- predict(gbm_try2, newdata=test_gbm, n.trees=400, type="response")
predictions_gbm2 <- ifelse(predict_gbm2 > 0.5, 1, 0)
predictions_gbm2 <- as.factor(predictions_gbm2)

# observe confusion matrix
summary(predictions_gbm2)
cm2 <- confusionMatrix(predictions_gbm2, test_gbm$relevance)
print(cm2)

#----------
# bernoulli needs numeric
training_copy <- training
training_copy$relevance <- as.numeric(as.factor(training$relevance)) - 1

# train on full training copies
gbm_try <- gbm(
  relevance ~ .,
  data = training_copy,
  distribution = "bernoulli",
  n.trees = 200,
  interaction.depth = 6,
  shrinkage = 0.075,
  n.minobsinnode = 20
)

gbm_try2 <- gbm(
  relevance ~ .,
  data = training_copy,
  distribution = "bernoulli",
  n.trees = 400,
  interaction.depth = 4,
  shrinkage = 0.08,
  n.minobsinnode = 20
)

summary(gbm_try) # get relative influence plots
summary(gbm_try2)