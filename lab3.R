# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  MEACHINE LEARNING & VISULISATIONS
#
# Practical Business Analytics
# Dept. of Computer Science
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# UPDATE
# 1.00      15/2/2019    Initial Version
# 1.01      25/2/2019    Updates for MANM module
# 1.02      16/10/2019   COM3018 / COMM053 2019
# 1.03      22/10/2019   Added PerformanceAnalytics as a required library
# 1.04      12/10/2020   Updated for R 4.x
# 1.05      26/11/2020   Changed "discreet" to "discrete" [woops!]
# ************************************************
# R Script For lab 3

#  clears all objects in "global environment"
rm(list=ls())

# ************************************************
# Global Environment variables
# - i.e. available to all functions
# Good practice to place "constants" in named variables
# I use UPPERCASE to identify these in my code

DATASET_FILENAME  <- "UCI-G.csv"          # Name of input dataset file
OUTPUT_FIELD      <- "Status"             # Field name of the output class to predict

HOLDOUT           <- 70                   # % split to create TRAIN dataset

SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection
                                          # Set to negative means analyse but do not replace outliers

TYPE_DISCRETE     <- "DISCRETE"           # field is discrete (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

DISCRETE_BINS     <- 6                    # Number of empty bins to determine discrete
MAX_LITERALS      <- 55                   # Maximum number of 1-hot ecoding new fields

# ************************************************
# Define and then load the libraries used in this project

# Library from CRAN     Version
# ************************************************
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "PerformanceAnalytics")

# User defined functions are next

# ************************************************
# NreadDataset() :
#
# Read a CSV file from working directory
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
NreadDataset<-function(csvFilename){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# myPerformancePlot() :
#
# Use dataset to generate predictions from model
# as classifier at range of thresholds values
# Plot the results
#
# INPUT   :   vector double  - probs        - probability of being class 1
#         :   Data Frame     - testing_data - dataset to evaluate
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
myPerformancePlot<-function(probs,testing_data){

  print("In my performance plot")

  # ************************************************
  # Q23
  toPlot<-data.frame()

  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-myEvaluateClassifier(probs=probs,testing_data=testing_data,threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }

  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1

  # Q25 use which.max() to return a single index to the higest value in the vector
  maxYoudan<-toPlot$x[which.max(toPlot$youdan)]

  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+(toPlot$fpr^2))

  minEuclidean<-toPlot$x[which.min(toPlot$distance)]

  plot(x=toPlot$x,y=toPlot$tpr, type="l",col="blue",
       xlab="Threshold",
       ylab="%Rate",
       main="Threshold Perfomance Loan Model")

  # Plot the Euclidean distance to "perfect" classifier (smallest the best)
  lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
  abline(v=minEuclidean,col="green",lty=3,lwd=2)

  # Plot the specificity (1-FPR)
  lines(x=toPlot$x,y=100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)

  # The point where specificity and sensitivity are the same
  crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
  abline(v=crosspoint,col="red",lty=3,lwd=2)

  # Q25 Plot Youdan distance
  lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
  abline(v=maxYoudan,col="purple",lty=3,lwd=2)

  legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
  text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nEuclidean=",minEuclidean,"\nYoudan=",maxYoudan))


  print("at end of performance plot")
}



# ************************************************
# myEvaluateClassifier() :
#
# Use dataset to generate predictions from model
# Evaluate as classifier using threshold value
#
# INPUT   :   vector double     - probs        - probability of being class 1
#             Data Frame        - testing_data - Dataset to evaluate
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
myEvaluateClassifier<-function(probs,testing_data,threshold) {

  predictedClass<-ifelse(as.numeric(probs)<threshold,0,1)
  expectedClass<-testing_data[,OUTPUT_FIELD]

  results<-NcalcConfusion(expectedClass=expectedClass,
                          predictedClass=predictedClass)

  return(results)
} #endof myEvaluateClassifier()


# Q19: *********************************************
# myModelFormula() :
#
# Create formula for column names & given output
#
# INPUT   :   Data frame - dataset         - data
#         :   String     - fieldNameOutput - name of the output field
#
# OUTPUT  :   Formula    - R formula object
#
# ************************************************

myModelFormula<-function(dataset,fieldNameOutput){

  inputs<-paste(names(dataset)[which(names(dataset)!=fieldNameOutput)],collapse = "+")

  output<-paste(fieldNameOutput,"~")

  formular=as.formula(paste(output,inputs))

  return(formular)

} #endof myModelFormula()

# ************************************************
# myModelling() :
# Create a logistic regression classifier and evaluate
#
# INPUT       :   data frame - training_data - data to train the model
#             :   data frame - testing_data  - data to evaluate the model
#
# OUTPUT      :   None
# Exercise 6
# ************************************************
myModelling<-function(training_data,testing_data){

  print("this is my modelling function")

  # ************************************************
  # Q20 - call the formula function
  formular<-myModelFormula(dataset=training_data,fieldNameOutput=OUTPUT_FIELD)

  # Q21: Build a logistic regression classifier on training dataset
  logisticModel<-stats::glm(formular,data=training_data,family=quasibinomial)

  # Get probabilities of being class 1 from the classifier
  probabilities<-predict(logisticModel, testing_data,type="response")

  # ************************************************
  #Q22: Evaluate the classifier on test dataset
  threshold<-0.7
  results<-myEvaluateClassifier(probs=probabilities,
                                testing_data=testing_data,
                                threshold=threshold)

  # This outputs our results into the "Viewer" in RStudio
  NprintMeasures(results)

  # Plot FPR/TPR through threshold range
  results<-myPerformancePlot(probs=probabilities,testing_data=testing_data)

  print("finished function")
}

# ************************************************
# main() :
# main entry point to execute analytics
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# Keeps all objects as local to this function
# ************************************************
main<-function(){

  print("Inside main function")

  print(DATASET_FILENAME)

  loans<-NreadDataset(DATASET_FILENAME)

  field_types<-NPREPROCESSING_initialFieldType(loans)

  print(field_types)

  numeric_fields<-names(loans)[field_types=="NUMERIC"]
  symbolic_fields<-names(loans)[field_types=="SYMBOLIC"]

  number_of_numeric<-length(numeric_fields)
  number_of_symbolic<-length(symbolic_fields)

  print(paste("NUMERIC FIELDS=",number_of_numeric))
  print(numeric_fields)
  print(paste("SYMBOLIC FIELDS=",number_of_symbolic))
  print(symbolic_fields)

  NPREPROCESSING_prettyDataset(loans)

  # ************************************************
  # Q3
  field_types1<-NPREPROCESSING_discreteNumeric(dataset=loans,
                                               field_types=field_types,
                                               cutoff=DISCRETE_BINS)
  print(field_types1)

  results<-data.frame(field=names(loans),initial=field_types,types1=field_types1)
  print(formattable::formattable(results))

  # ************************************************
  # Q4

  ordinals<-loans[,which(field_types1==TYPE_ORDINAL)]
  ordinals<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)

  # Q6: ************************************************
  # z-scale
  zscaled<-as.data.frame(scale(ordinals,center=TRUE, scale=TRUE))

  # Q7: n the chosen classifier, the input values need to be scaled to [0.0,1.0]
  ordinalReadyforML<-Nrescaleentireframe(zscaled)

  # ************************************************
  # Q8: Process the catagorical (symbolic/discrete) fields using 1-hot-encoding
  catagoricalReadyforML<-NPREPROCESSING_categorical(dataset=loans,field_types=field_types1)

  print(formattable::formattable(data.frame(fields=names(catagoricalReadyforML))))

  # Q9: number of non-numeric fields before transformation
  # which fields are either SYMBOLIC or discrete
  nonNumericbefore<-length(which(field_types1!=TYPE_ORDINAL))

  # How many fields have be generated through the 1-hot-encoding process
  nonNumerictranformed<-ncol(catagoricalReadyforML)
  print(paste("Symbolic fields. Before encoding=",nonNumericbefore,"After",nonNumerictranformed))


  # Q11: ************************************************
  # Combine the two sets of data that are read for ML
  combinedML<-cbind(ordinalReadyforML,catagoricalReadyforML)

  # The dataset for ML information
  print(paste("Fields=",ncol(combinedML)))
  # Q14: Are any of the fields redundant?
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=OUTLIER_CONF)

  # Q12: The dataset for ML information
  print(paste("Fields=",ncol(combinedML)))

  # ************************************************
  # **** Create a TRAINING dataset using HOLDOUT% (e.g. 70) of the records

  # Q16: Randomise the entire data set - sample() is an easy way to do this!
  combinedML<-combinedML[sample(nrow(combinedML)),]

  # Q17: Create a TRAINING dataset using first HOLDOUT% of the records
  # and the remaining 30% is used as TEST
  # use ALL fields (columns)
  training_records<-round(nrow(combinedML)*(HOLDOUT/100))
  training_data <- combinedML[1:training_records,]
  testing_data = combinedML[-(1:training_records),]

  # Q18
  # numberOfBad<-nrow(subset(testing_data,Status==0))
  # numberOfgood<-nrow(subset(testing_data,Status==1))
  #print(paste("Number of good loan records in testing",numberOfgood))
  #print(paste("Number of bad loan records in testing",numberOfBad))



  myModelling(training_data = training_data, testing_data = testing_data)


  print("Leaving main")

} #endof main()

# ************************************************
# This is where R starts execution

# clears the console area
cat("\014")

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#This [optionally] sets working directory
#setwd("")

# Load additional R script files provide for this lab
# We normally use source(), but debugSource()  allows breakpoints in the code
# This file must be in the current working directory
debugSource("lab3dataPrep.R")

set.seed(123)

print("WELCOME TO LAB 3")

# ************************************************
main()

print("end")

