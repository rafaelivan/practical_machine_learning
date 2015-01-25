# Loads training and testing datasets from CSV files.
trainData <- read.csv('dataset/pml-training.csv')
testData <- read.csv('dataset/pml-testing.csv')

#
# Data exploration.
#

View(trainData)
str(trainData)
summary(trainData)

#
# Data cleaning
#

rm(list = ls()) # Clears the environment.

sanitizeData <- function(data) {
  rejectedCols <- c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2',
                    'cvtd_timestamp', 'new_window', 'num_window')
  
  data <- data[, -which(names(data) %in% rejectedCols)]
  return (data[, colSums(is.na(data)) == 0])
}

naStrings <- c(NA, '', '#DIV/0!')
trainData <- read.csv('dataset/pml-training.csv', na.strings = naStrings)
testData <- read.csv('dataset/pml-testing.csv', na.strings = naStrings)

trainData <- sanitizeData(trainData)
testData <- sanitizeData(testData)

#
# Building the cross validation set
#

library('caret')

inTrain <- createDataPartition(trainData$classe, p = 0.6, list = F)
validationData <- trainData[-inTrain,]
trainData <- trainData[inTrain,]

#
# Modeling
#

modelFit <- train(trainData$classe ~ ., data = trainData, method = 'rf')

#
# Cross validation
#

predictions <- predict(modelFit, validationData)
predictions <- as.data.frame(predictions)
predictions$correct <- validationData$classe
confusionMatrix(predictions$predictions, predictions$correct)