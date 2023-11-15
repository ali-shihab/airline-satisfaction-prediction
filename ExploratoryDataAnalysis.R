# ************************************************
# ************************************************
# Ali - Group 25 - Exploratory Data Analysis script
#
# This script contains the exploratory data analysis
# as a preliminary to the subsequent Modelling.R
# script. This was also assisted by the following
# functions from the PBA lab3DataPreprocessing.R script:
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
  NPREPROCESSING_prettyDataset(data)
  
  # determine number of unique values of each field
  getUniqueValues(data)
  
  # determine field types
  fieldTypes<-getColumnTypes(data)
  
  # display descriptive statistics
  NPREPROCESSING_prettyDataset(data)
  
  # visualise discrete data
  fieldTypes1<-NPREPROCESSING_discreteNumeric(dataset=data,
                                               field_types=fieldTypes,
                                               cutoff=DISCRETE_BINS)
  print("Field types after discrete-binning:/n")
  print(fieldTypes1)
  
  results<-data.frame(field=names(data),initial=fieldTypes,types1=fieldTypes1)
  print(formattable::formattable(results))
  
  # visualise distributions of data
  histPlots <-visualiseHist(data)
  print(histPlots)
  
  # importance of each field via randomforest
  
  
  # enhanced visualisation of distribution, correlation
  # and plot of pairs 
  pairPlot <- ggpairs(data)
  print(pairPlot)
  
  # heatmap of correlations
  
  # check missing values
  
  
  # ************************************************
  # PRE-PROCESSING & VISUALISATION
  # ************************************************
  
  # deal with missing values
  
  # encode categoricals
  #data <- encodeCategoricals(data)
  
  # standardise/scale data
  
  # outlier analysis
  # naive
  #ordinals<-data[,which(fieldTypes1==TYPE_ORDINAL)]
  #ordinals<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)
  
  # LOF outlier removal compared to Random Forest
  
  
  
  # descriptive summary of structure & statistics
  # post-encoding of categoricals
  #str(data)
  #summary(data)
  
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
               "tidyr",
               "ggplot2",
               "GGally",
               "dplyr",
               "PerformanceAnalytics")
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

# load pre-processing helper functions
source("Preprocess.R")
source("lab3DataPrep.R")

# seed for reproducibility
set.seed(123)

# ************************************************
main()
print("End of EDA")