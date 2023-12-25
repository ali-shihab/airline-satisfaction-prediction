# ************************************************
# ************************************************
# Ali - Group 25 - Data Pre-procesisng script.
#
# This script contains the custom helper functions
# necessary for my associated ExploratoryDataAnalysis.R
# script. This was also assisted by the following
# functions from the PBA lab3DataPreprocessing.R script:
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
               "PerformanceAnalytics")
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
  testData<-read.csv(test,encoding="UTF-8",stringsAsFactors = FALSE)
  
  # combine the datasets
  combined <- rbind(trainData, testData)
  
  # remove non-alphanumeric characters
  names(combined)<-NPREPROCESSING_removePunctuation(names(combined))
  
  print(paste("CSV data read. Records=",nrow(combined)))
  return(combined)
}
# ************************************************
# GET COLUMN TYPES
# ************************************************
getColumnTypes <- function(df) {
  # Check if the input is a dataframe
  if(!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  # Initialize an empty list to store the types
  typesList <- list()
  
  # Loop through each column of the dataframe
  for (i in seq_along(df)) {
    # Get the type of the current column and add it to the list
    typesList[[names(df)[i]]] <- ifelse(is.numeric(df[,i]), 
                                        TYPE_NUMERIC, 
                                        TYPE_SYMBOLIC)
  }
  
  # split according to type
  numericFields <- Filter(function(x) x==TYPE_NUMERIC, typesList)
  symbolicFields <- Filter(function(x) x==TYPE_SYMBOLIC, typesList)
  
  # determine number of each type
  numberOfNumeric<-length(numericFields)
  numberOfSymbolic<-length(symbolicFields)
  
  # print number of each type
  print(paste("number of numeric fields=",numberOfNumeric))
  print(numericFields)
  print(paste("number of symbolic fields=",numberOfSymbolic))
  print(symbolicFields)
  
  return(typesList)
}
# ************************************************
# GET UNIQUE VALUES OF EACH COLUMN
# ************************************************
getUniqueValues<-function(df) {
  uniqueCounts <- sapply(df, function(x) length(unique(x)))
  return(uniqueCounts)
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
    
    if (!is.character(col)){
      ggplot(dataFrame, aes(x = !!sym(col))) + 
        geom_histogram(bins = 30, fill = 'black', color = 'black') +
        theme_minimal() +
        ggtitle(paste("Histogram of", col))
      
    } else {
    
      ggplot(dataFrame, aes(x = !!sym(col))) + 
        geom_bar(fill = 'black', color = 'black') +
        theme_minimal() +
        ggtitle(paste("Histogram of", col))
    }
    
  })
  
  return(histPlots)
}



# One-hot encoding categorical features
#combined <- dummyVars("~ .", data = combined) %>% predict(combined)

# Standardize the data with z-score
#combined_scaled <- scale(combined[, sapply(combined, is.numeric)])

# Local Outlier Factor (LOF) Analysis
#lof <- lofactor(combined_scaled)
#outliers <- which(lof > 1.5) # Assuming threshold for outliers
#combined_cleaned <- combined[-outliers,]

# Random Forest for further outlier detection and removal
#rf_model <- randomForest(satisfaction ~ ., data=combined_cleaned)
#importance <- importance(rf_model)
#varImpPlot(rf_model)

# Stratified train-test split
#set.seed(123)
#split <- createDataPartition(combined_cleaned$satisfaction, p = 0.8, list = FALSE)
#train_set <- combined_cleaned[split,]
#test_set <- combined_cleaned[-split,]

# Setting up stratified k-fold cross-validation on training data
#folds <- createFolds(train_set$satisfaction, k = 10)

# Confirming binary classification
#unique(train_set$satisfaction)