# Load necessary libraries
library(tidyverse)
library(caret)
library(dplyr)
library(randomForest)
library(DMwR)

# Load the datasets
train <- read.csv("path/to/train.csv")
test <- read.csv("path/to/test.csv")



# Combine the datasets
combined <- rbind(train, test)



# One-hot encoding categorical features
combined <- dummyVars("~ .", data = combined) %>% predict(combined)

# Standardize the data with z-score
combined_scaled <- scale(combined[, sapply(combined, is.numeric)])

# Local Outlier Factor (LOF) Analysis
lof <- lofactor(combined_scaled)
outliers <- which(lof > 1.5) # Assuming threshold for outliers
combined_cleaned <- combined[-outliers,]

# Random Forest for further outlier detection and removal
rf_model <- randomForest(satisfaction ~ ., data=combined_cleaned)
importance <- importance(rf_model)
varImpPlot(rf_model)

# Stratified train-test split
set.seed(123)
split <- createDataPartition(combined_cleaned$satisfaction, p = 0.8, list = FALSE)
train_set <- combined_cleaned[split,]
test_set <- combined_cleaned[-split,]

# Setting up stratified k-fold cross-validation on training data
folds <- createFolds(train_set$satisfaction, k = 10)

# Confirming binary classification
unique(train_set$satisfaction)