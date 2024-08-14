install.packages("randomForest")
library(randomForest)

# set up for 5-fold CV, change m each time
train_control <- trainControl(method = "cv", number = 5)
tune_grid <- expand.grid(
  mtry = c(1, 2, 3, 4, 5)
)

# train on base data
rf_model <- train(
  relevance~.,
  data = training,
  method = "rf",
  trControl = train_control,
  tuneGrid = tune_grid,
  ntree = 100,
  importance = TRUE
)

print(rf_model$bestTune) # mtry = 2
print(rf_model)

# collect results
results_rf <- rf_model$results
results_rf$error_rate <- 1 - results_rf$Accuracy

# ---------Trying interaction terms

# initial training was very slow, can only try a few terms
# Choosing highest correlations with most signals included (Sig3 has strong)

tune_grid <- expand.grid(
  mtry = c(1, 2, 3, 4, 5)
)

training_int <- training
# Log scale to prevent large values in interaction terms
log_sigs <- c("sig3", "sig5", "sig6")
# use standardize custom
training_int <- standardize_custom(training_int, scale_sigs = c(), log_sigs = log_sigs)
# chosen by highest correlation that gets 5 signals involved
training_int$sig3_sig5 <- training_int$sig3 * training_int$sig5
training_int$sig2_sig7 <- training_int$sig2 * training_int$sig7
training_int$sig5_sig6 <- training_int$sig5 * training_int$sig6

# fit models
rf_model_int <- train(
  relevance~.,
  data = training_int,
  method = "rf",
  trControl = train_control,
  tuneGrid = tune_grid,
  ntree = 100,
  importance = TRUE
)

# collect results
print(rf_model_int$bestTune) # mtry = 2
print(rf_model_int)

#minimal improvement
results_rf_int <- rf_model_int$results
results_rf_int$error_rate <- 1 - results_rf_int$Accuracy

# ----------------- Plot Both

results_rf$model <- "Regular Data"
results_rf_int$model <- "Interaction Terms"
combined_results_rf <- rbind(results_rf, results_rf_int)

ggplot(combined_results_rf, aes(x = mtry, y = error_rate, color = model)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Adding Interaction Terms to Random Forest",
    x = "M",
    y = "Error Rate",
  ) +
  theme_minimal() +
  scale_color_manual(values = c("maroon", "limegreen")) +
  coord_cartesian(ylim = c(0.33, 0.38))

# very little affect

# check variable importance
var_imp <- varImp(rf_model_int)
print(var_imp)
# Sig4 with 0??? Sig2/Sig7 and Sig5/Sig6 relevant

# ---------------- Vary # of Trees

tune_grid <- expand.grid(
  mtry = c(2)
)

# n = 300
rf_model_300 <- train(
  relevance~.,
  data = training_int,
  method = "rf",
  trControl = train_control,
  tuneGrid = tune_grid,
  ntree = 300,
  importance = TRUE
)

# n = 500
rf_model_500 <- train(
  relevance~.,
  data = training_int,
  method = "rf",
  trControl = train_control,
  tuneGrid = tune_grid,
  ntree = 500,
  importance = TRUE
)

print(rf_model_300) #5-Fold CV Accuracy: 66.41%
print(rf_model_500) #5-Fold CV Accuracy: 66.33%

# Fit (out of box) w/ n = 300, m = 2 or 4
training <- read.csv("training.csv")
test <- read.csv("test.csv")
training <- training[,3:13]
test <- test[, 3:12]
training$is_homepage <- as.factor(training$is_homepage)
test$is_homepage <- as.factor(test$is_homepage)
training$relevance <- as.factor(training$relevance)

rf_m2 <- randomForest(relevance~., training, ntree=300, mtry=2)
rf_m4 <- randomForest(relevance~., training, ntree=300, mtry=4)

rf2_predictions <- generate_predictions(rf_m2, test)
rf4_predictions <- generate_predictions(rf_m4, test)