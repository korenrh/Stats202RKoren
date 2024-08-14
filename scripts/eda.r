library(ISLR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(car)
training <- read.csv("training.csv")
test <- read.csv("test.csv")

sum(is.na(training)) # 0
n <- nrow(training) # 80,046
summary(training)

length(unique(training$query_id)) # 12408 unique
length(unique(training$url_id)) # 75231 unique
# id: unique for each observation
length(unique(training$id)) #80046

# Drop IDs
training <- training[,3:13]
test <- test[,3,12]

# 43.71% are relevant
summary(training$relevance)

# Correlation Values
cors <- cor(training)
print(cors)

# Correlation Heat Map
ggplot(data = melt(cors), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We see a range of correlations between each signal and relevance
# Sig3 and Sig5 most notable at 0.8

# fit basic linear model
lm <- lm(relevance ~ ., data = training)

# view VIF and basic summary
vif(lm)
summary(lm)
# Some collinearity between sig3/sig5
# Sig1/2 and Sig6/7/8 are most significant
# query_length may be significant

# Query Length: Range 1-18, Median: 2
# Is Homepage: 0 or 1, Mean: 0.2689 (26.89% are homepages), convert to factor
training$is_homepage <- as.factor(training$is_homepage)
test$is_homepage <- as.factor(test$is_homepage)

summary(training) # View ranges, means, medians
# Sig1: Range 0-1, Median: 0.15, Mean: 0.1832
# Sig2: Range 0-0.86, Median: 0.34, Mean: 0.3469
# Sig3: Range 0-673637, Median: 417, Mean: 4857
# Sig4: Range 0-660939, Median: 220, Mean: 742.3
# Sig5: Range 0-46994, Median: 64, Mean: 550.5
# Sig6: Range 0-3645, Median: 0, Mean: 14.1
sum(training$sig6==0)/n # 65% of Sig6 is 0
# Sig7: Range 0-0.88, Median: 0.31, Mean: 0.3195
# Sig8: Range 0-0.94, Median: 0.46, Mean: 0.4718
# Relevance: 0 or 1, Mean: 0.4371 (43.71% are relevant) convert to factor
training$relevance <- as.factor(training$relevance)

# Let's plot the signal variable values to see their distribution
# making histograms for each signal and query length
par(mfrow = c(2, 4))
hist(training$sig1, main = "Sig1", xlab = "Value", col = "blue")
hist(training$sig2, main = "Sig2", xlab = "Value", col = "red")
hist(training$sig3, main = "Sig3", xlab = "Value", col = "purple")
hist(training$sig4, main = "Sig4", xlab = "Value", col = "orange")
hist(training$sig5, main = "Sig5", xlab = "Value", col = "pink")
hist(training$sig6, main = "Sig6", xlab = "Value", col = "green")
hist(training$sig7, main = "Sig7", xlab = "Value", col = "brown")
hist(training$sig8, main = "Sig8", xlab = "Value", col = "yellow")

# apply log(1+x) to normalize sig3-6
par(mfrow = c(1, 4))
hist(log1p(training$sig3), main = "Sig3", xlab = "log(Sig3+1)", col = "purple")
hist(log1p(training$sig4), main = "Sig4", xlab = "log(Sig4+1)", col = "orange")
hist(log1p(training$sig5), main = "Sig5", xlab = "log(Sig5+1)", col = "pink")
hist(log1p(training$sig6), main = "Sig6", xlab = "log(Sig6+1)", col = "green")

# check out query_length distribution
hist(training$query_length, main = "Query Length", xlab = "Value", col = "red")

# From plotting each signal variable, we can see that Sig3-Sig6 have a wide range of values that are not normally distributed
# Sig6 in particular has a value of 0 for 65% of observations so we'll need to account for that as well

# Looking at the values at top deciles to interpret our signals.
# Does a really high or low signal strongly imply or reject relevance?

# calculate relevance % for signals in each decile
calculate_decile_relevance <- function(data, signal) {
  breaks <- seq(0, 1, by = 0.1)
  results <- lapply(seq_along(breaks)[-length(breaks)], function(i) {
    lower <- breaks[i]
    upper <- breaks[i + 1]
    lower_threshold <- quantile(data[[signal]], lower, na.rm = TRUE)
    upper_threshold <- quantile(data[[signal]], upper, na.rm = TRUE)
    
    deciles <- data %>%
      filter(get(signal) >= lower_threshold & get(signal) <= upper_threshold)
    
    relevance_pct <- mean(deciles$relevance == 1, na.rm = TRUE) * 100
    return(data.frame(decile_range = paste0(sprintf("%.0f", lower * 100), "%-", sprintf("%.0f", upper * 100), "%"), relevance_pct = relevance_pct))
  })
  df <- bind_rows(results)
  df$signal <- signal
  return(df)
}

# check all signals
signals <- c("sig1", "sig2", "sig3", "sig4", "sig5", "sig6", "sig7", "sig8")
results <- lapply(signals, function(signal) calculate_decile_relevance(training, signal))
results_df <- bind_rows(results)

# plot decile %s as line graph
ggplot(results_df, aes(x = decile_range, y = relevance_pct, color = signal, group = signal)) +
  geom_line() +
  geom_point() +
  labs(
    title = "% of Relevant Observations by Decile",
    x = "Decile Range",
    y = "% Relevance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Sig2 and Sig6 stand out as the most informative individual signals
# These results suggest higher signals do imply better odds of relevance
# for all but Sig8, yet there is not a "magic bullet" for predicting relevance.

# --- Scaling

# Scale our signals, both to bring the magnitudes to similar values and
# to account for its high proportion of zeroes in some of the signals

# See standardize.r for standardize functions, we implement three approaches:
# 1. Scale each variable and replace the feature with the scaled feature
# Use cases: Situations where feature magnitude can affect the model, need limited # of features
# 2. Scale each signal and add the feature to the feature space
# Use cases: Situations where feature magnitude may not affect the model as much and feature selection will be applied to choose the best
# 3. Mix and match log, scaling, and possibly other transformations
# Use cases: Other scenarios: exploring fit improvement, alternative transformations, unique distributions/relationships

#-----Reset data before standardizing
# convert columns as needed
training <- read.csv("training.csv")
test <- read.csv("test.csv")

# add factors back
training$is_homepage <- as.factor(training$is_homepage)
training$relevance <- as.factor(training$relevance)
test$is_homepage <- as.factor(test$is_homepage)

# cut ID columns
training<-training[,3:13]
test<-test[,3:12]

# standardize our signals
s_sigs <- c("sig1", "sig2", "sig3", "sig4", "sig5", "sig6", "sig7", "sig8", "query_length")
train_std <- standardize(training, s_sigs)
test_std <- standardize(test, s_sigs)
summary(train_std)

# plot new distributions for training
par(mfrow = c(2, 4))
hist(train_std$sig1, main = "Sig1", xlab = "Value", col = "blue")
hist(train_std$sig2, main = "Sig2", xlab = "Value", col = "red")
hist(train_std$sig3, main = "Sig3", xlab = "Value", col = "purple")
hist(train_std$sig4, main = "Sig4", xlab = "Value", col = "orange")
hist(train_std$sig5, main = "Sig5", xlab = "Value", col = "pink")
hist(train_std$sig6, main = "Sig6", xlab = "Value", col = "green")
hist(train_std$sig7, main = "Sig7", xlab = "Value", col = "brown")
hist(train_std$sig8, main = "Sig8", xlab = "Value", col = "yellow")

# plot new distributions for test
par(mfrow = c(2, 4))
hist(test_std$sig1, main = "Sig1", xlab = "Value", col = "blue")
hist(test_std$sig2, main = "Sig2", xlab = "Value", col = "red")
hist(test_std$sig3, main = "Sig3", xlab = "Value", col = "purple")
hist(test_std$sig4, main = "Sig4", xlab = "Value", col = "orange")
hist(test_std$sig5, main = "Sig5", xlab = "Value", col = "pink")
hist(test_std$sig6, main = "Sig6", xlab = "Value", col = "green")
hist(test_std$sig7, main = "Sig7", xlab = "Value", col = "brown")
hist(test_std$sig8, main = "Sig8", xlab = "Value", col = "yellow")

# standardize with extra features
train_std_extra <- standardize_add_features(training)
test_std_extra <- standardize_add_features(test)
summary(train_std_extra)

# standardize with custom features
s_sigs <- c("sig3", "sig4", "sig5")
l_sigs <- c("sig6")
train_std_custom <- standardize_custom(training, s_sigs, l_sigs)
summary(train_std_custom)

#----Extra set-up for CV and predictions
# set up for K-Fold as needed
k <- 5
kfolds <- sample(1:k, nrow(training), replace=T)

# split data once to practice making predictions
set.seed(24)
n <- nrow(training)
train_split <- sample(seq_len(n), size=n*.7)
my_train <- training[train_split, ]
my_test_std <- training[-train_split, ]
my_train_std <- train_std[train_split, ]
my_test_std <- train_std[-train_split, ]
my_train_cut <- train_std_cut[train_split, ]
my_test_cut <- train_std_cut[-train_split, ]