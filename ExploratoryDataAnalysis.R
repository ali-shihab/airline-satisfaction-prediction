# ************************************************
# ************************************************
# Ali - Group 25 - Exploratory Data Analysis script
#
# This script contains the exploratory data analysis
# as a preliminary to the subsequent Modelling.R
# script. This was also assisted by the following
# functions from the PBA labXDataPreprocessing.R script:
# - N
# ************************************************
# ************************************************
# clear all environment variables

rm(list=ls())

# ************************************************
# CONSTANTS
# ************************************************

# datasets - these were split into train and test 
# by the original provider, and will be combined
TRAIN_FILENAME <- "train.csv"
TEST_FILENAME <- "test.csv"
TARGET_FIELD <- "satisfaction"

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
# NPREPROCESSING_categorical() :
#
# Transform SYMBOLIC or DISCRETE fields using 1-hot-encoding
#
# INPUT: data frame    - dataset      - symbolic fields
#        vector string - field_types  - types per field {ORDINAL, SYMBOLIC, DISCRETE}
#
# OUTPUT : data frame    - transformed dataset
#
# 18/2/2021 NRT Updated for efficiency
# ************************************************

NPREPROCESSING_categorical<-function(dataset,field_types){
  
  catagorical<-data.frame()
  
  categorical_fields<-names(dataset)[which(field_types==TYPE_SYMBOLIC | field_types==TYPE_DISCRETE)]
  
  # for each field
  for (field in categorical_fields){
    
    # Convert into factors. A level for each unique string
    ffield<-factor(dataset[,field])
    
    # Check if too many unique values to encode
    if (nlevels(ffield) > MAX_LITERALS) {
      stop(paste("Practical Business Analytics - too many literals in:",
                 field,
                 nlevels(ffield)))
    }
    
    # Check if just one value!
    if (nlevels(ffield) ==1) {
      stop(paste("Practical Business Analytics - field stuck at a single value:",
                 field))
    }
    
    # 1-hot encoding. A new column for each unique "level"
    xx<-data.frame(model.matrix(~ffield+0, data=ffield))
    
    names(xx)<-gsub("ffield",field,names(xx))
    
    # If 2 unique values, then can encode as a single "binary" column
    if (ncol(xx)==2){
      xx<-xx[,-2,drop=FALSE]
      names(xx)<-field  # Field name without the value appended
    }
    
    catagorical<-as.data.frame(append(catagorical,xx))
    
  } #endof for()
  return (catagorical)
  
} # endof categorical_encoding()

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
  
  # ************************************************
  # EDA
  # ************************************************
  print("Starting EDA")
  
  # descriptive summary of structure & statistics
  # pre-encoding of categoricals
  str(data)
  summary(data)
  
  # visualise distributions of data
  histPlots <-visualiseHist(data)
  
  # visualise discrete data
  
  # enhanced visualisation of distribution, correlation
  # and plot of pairs 
  pairPlot <- ggpairs(data)
  
  # heatmap of correlations
  
  # check missing values
  
  # outlier analysis
  
  # ************************************************
  # PRE-PROCESSING & VISUALISATION
  # ************************************************
  # encode categoricals
  data <- encodeCategoricals(data)
  
  # descriptive summary of structure & statistics
  # post-encoding of categoricals
  str(data)
  summary(data)
  
  # standardise/scale data
  
  # LOF outlier removal compared to Random Forest
  # outlier removal
  
  # visualisation of outlier removal
  
  # if linear relationships, PCA & potentially MCA or 
  # CATPCA, 80%/85% Var. Exp.
  
  # compare with autoencoder
  
  # compare with t-SNE and UMAP
  
  # ************************************************
  # MODELLING, METRICS & VISUALISATIONS - DIFFERENT FILE
  # ************************************************
  
  # Deep VAE for clustering
  
  # HDBScan or Kmeans depending on data
  

  
  

  

  

  

  
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
               "tidyverse",
               "tidyr",
               "ggplot2",
               "GGally",
               "dplyr",
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