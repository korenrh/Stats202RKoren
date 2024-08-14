set.seed(7)
library(dplyr)
library(caret)
library(ggplot2)

training$is_homepage <- as.factor(training$is_homepage)
training$relevance <- as.factor(training$relevance)

# set up for 5-Fold CV
train_control <- trainControl(method = "cv", number = 5)
# range of K
tune_grid <- expand.grid(k = c(5, 25, 45, 65, 85))

knn_model <- train(
  relevance~.,
  data = training,
  method = "knn",
  trControl = train_control,
  tuneGrid = tune_grid)

print(knn_model$bestTune) #k = 85
print(knn_model)

# calculate results
results <- knn_model$results
results$error_rate <- 1 - results$Accuracy


# ---------- Standardize data
s_sigs <- c("sig1", "sig2", "sig3", "sig4", "sig5", "sig6", "sig7", "sig8", "query_length")
train_std <- standardize(training, s_sigs)
summary(train_std)

# fit on standardized data
knn_model_std <- train(
    relevance~.,
    data = train_std,
    method = "knn",
    trControl = train_control,
    tuneGrid = tune_grid)

# get best results
print(knn_model_std$bestTune)
print(knn_model_std)

# collect error rates
results_std <- knn_model_std$results
results_std$error_rate <- 1 - results_std$Accuracy

# ----------------- Plot Both

results$model <- "Raw Data"
results_std$model <- "Standardized"
combined_results <- rbind(results, results_std)

ggplot(combined_results, aes(x = k, y = error_rate, color = model)) +
  geom_line() +
  geom_point() +
  labs(
    title = "K-Nearest Neighbors on Raw and Standardized Data",
    x = "K",
    y = "Error Rate",
  ) +
  theme_minimal() +
  scale_color_manual(values = c("maroon", "limegreen")) +
  coord_cartesian(ylim = c(0.33, 0.45))

# Standardization reduces error rate by nearly 8%!

# -----------------
# try higher K

train_control <- trainControl(method = "cv", number = 5)
tune_grid <- expand.grid(k = c(105, 125, 145, 165))

knn_model_high <- train(
  relevance~.,
  data = train_std,
  method = "knn",
  trControl = train_control,
  tuneGrid = tune_grid)

print(knn_model_high$bestTune)
print(knn_model_high)

results_high <- knn_model_high$results
results_high$error_rate <- 1 - results_high$Accuracy

# combine results
results_full <- rbind(results_std, results_high)

ggplot(results_full, aes(x = k, y = error_rate)) +
  geom_line(color = "limegreen") +
  geom_point(color = "limegreen") +
  labs(title = "KNN Error Rates (standardized signals, 5-Fold CV)",
       x = "K",
       y = "Error Rate",) +
  theme_minimal()
  
# similar results

# Future: Generate KNN predictions w/ all scaled
s_sigs <- c("sig1", "sig2", "sig3", "sig4", "sig5", "sig6", "sig7", "sig8", "query_length", "is_homepage")
train_std <- standardize(training, s_sigs)
train_std <- train_std[ , 3:13]
test_std <- standardize(test, s_sigs)
