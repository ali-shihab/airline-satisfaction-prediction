# ************************************************
# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
# Ali - Group 25 - Exploratory Data Analysis
# ************************************************
# clear all environment variables
rm(list=ls())

# ************************************************
# CONSTANTS

# datasets - these were split into train and test 
# by the original provider, and will be combined
TRAIN_FILENAME <- "train.csv"
TEST_FILENAME <- "test.csv"
TARGET_FIELD <- "satisfaction"

# holdout - even split of 70/30 and 80/20 in 
# literature and industry, so 75/25 used
HOLDOUT <- 70 #
# ************************************************
# HELPER FUNCTIONS - MOVE TO PREPROCESSING LIBRARY
# ************************************************
# READ DATA FUNCTION
# ************************************************
readData<-function(train, test) {
  
  # read both train and test datasets
  trainData<-read.csv(train,encoding="UTF-8",stringsAsFactors = FALSE)
  testData<-read.csv(train,encoding="UTF-8",stringsAsFactors = FALSE)
  
  # Combine the datasets
  combined <- rbind(trainData, testData)
  
  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  names(combined)<-NPREPROCESSING_removePunctuation(names(combined))
  
  print(paste("CSV data read. Records=",nrow(combined)))
  return(combined)
}
# ************************************************
# main() :
# main entry point to execute analytics
#
# INPUT : None
#
# OUTPUT : None
#
# ************************************************
main<-function(){
  
  print("Inside main function")
  
  # read in data and combine
  data <- readData(TRAIN_FILENAME, TEST_FILENAME)
  
  # descriptive summary of structure & statistics
  str(data)
  summary(data)
  
  
  print("Leaving main")
  
}
# ************************************************

# clears console
cat("\014")

# Loads the libraries
MYLIBRARIES<-c("outliers",
               "caret",
               "randomForest",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "PerformanceAnalytics")
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

# load pre-processing helper functions
source("lab3DataPrep.R")

# seed for reproducibility
set.seed(123)

# ************************************************
main()
print("End of EDA")