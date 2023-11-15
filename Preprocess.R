# ************************************************
# ************************************************
# Ali - Group 25 - Data Pre-procesisng script.
#
# This script contains the custom helper functions
# necessary for my associated ExploratoryDataAnalysis.R
# script. This was also assisted by the following
# functions from the PBA labXDataPreprocessing.R script:
# - N
# ************************************************
# ************************************************
# Load necessary libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)
MYLIBRARIES<-c("outliers",
               "caret",
               "randomForest",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "tidyr",
               "ggplot2",
               "GGally",
               "dplyr",
               "PerformanceAnalytics",
               "DMwR")
# ************************************************
# CONSTANTS

# ************************************************
# HELPER FUNCTIONS - MOVE TO PREPROCESSING LIBRARY
# ************************************************
# READ DATA FUNCTION
# ************************************************
readData<-function(train, test) {
  
  # read both train and test datasets
  trainData<-read.csv(train,encoding="UTF-8",stringsAsFactors = FALSE)
  testData<-read.csv(train,encoding="UTF-8",stringsAsFactors = FALSE)
  
  # combine the datasets
  combined <- rbind(trainData, testData)
  
  # remove non-alphanumeric characters
  names(combined)<-NPREPROCESSING_removePunctuation(names(combined))
  
  print(paste("CSV data read. Records=",nrow(combined)))
  return(combined)
}
# ************************************************
# ENCODE CATEGORICALS
# ************************************************
encodeCategoricals<-function(dataFrame){

  encodedCategoricalDataFrame <- as.data.frame(dummyVars(
    "~ .", 
    data = dataFrame)) %>% predict(dataFrame)
  
  return(encodedCategoricalDataFrame)
}
# ************************************************
# HISTOGRAM VISULISATIONS OF COLUMNS
# ************************************************
# numeric_cols <- sapply(data, is.numeric)
# data_num <- data[, numeric_cols]
visualiseHist<-function(dataFrame){
  
  histPlots <- lapply(names(dataFrame), function(col) {
    
    ggplot(dataFrame, aes_string(x = col)) + 
      geom_histogram(bins = 30, fill = 'blue', color = 'black') +
      theme_minimal() +
      ggtitle(paste("Histogram of", col))
    
  })
  
  return(histPlots)
}



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