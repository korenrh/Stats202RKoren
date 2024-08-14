install.packages("e1071")
library(e1071)
library(dplyr)
set.seed(7) # Tried many seeds

# set up tune_grid
costs <- c(0.1, 1, 10, 100) # initial range
gammas <- c(0.1) # 1 / features
tune_grid <- expand.grid(
  cost = costs,
  gamma = gammas
)

# load in data
training <- read.csv("training.csv")
training$relevance <- as.factor(training$relevance)

# split and prepare data
n <- nrow(training)
train_size <- floor(n * 0.7)
train_i <- sample(seq_len(n), size = train_size)
train_svm <- training[train_i, ]
test_svm <- training[-train_i, ]
my_train <- train_svm[, 3:13]
my_test <- test_svm[, 3:13]

# standardize to account for magnitude
sigs <- c("sig1","sig2","sig3","sig4","sig5","sig6","sig7","sig8","query_length","is_homepage")
my_train_std <- standardize(my_train, sigs)
my_test_std <- standardize(my_test, sigs)

# fit with training split
svm <- svm(my_train_std[,1:10], my_train_std[,11], cost=10, gamma=0.1)

# predict and test
svm_predict <- predict(svm, newdata=my_test_std[,1:10])
summary(svm_predict)
svm_errors <- svm_predict != my_test_std$relevance
mean(svm_errors) * 100 # 32.98% in this case
table(svm_predict, my_test_std$relevance)

# standardize full training data
training <- read.csv("training.csv")
# training$is_homepage <- as.factor(training$is_homepage)
training$relevance <- as.factor(training$relevance)
train_std <- standardize(training, sigs)
svm <- svm(train_std[,3:12], train_std[,13], cost=10, gamma=0.1)

# standardize full test data (even homepage)
test <- read.csv("test.csv")
test <- test[,3:12]
test_std <- standardize(test, sigs)

# check test and predict
summary(test_std)
svm_predict <- predict(svm, newdata=test_std)

# confusion matrix
summary(svm_predict)

# see Test.R for producing submission

# attempt tune SVM (runs without end)
set.seed(7)
svm_tune <- tune(
  svm,
  relevance~.,
  train_std,
  ranges = tune_grid
)

# Apply transformations? Feature Interactions?
# - Replace sig4 with Sig5-Sig6 or Sig2-Sig7?