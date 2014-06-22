#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))

TRAINING_FILE <- "pml-training.csv"
TESTING_FILE <- "pml-testing.csv"
DEBUG <- FALSE

main <- function() {
  set.seed(123)
  
  ## Load the data
  fr <- loadFile(TRAINING_FILE)
  frFinalTest <- loadFile(TESTING_FILE)
  
  ## Clean-up them
  fr <- cleanUp(fr)
  frFinalTest <- cleanUp(frFinalTest)

  ## Plot an example of a variable
  plotHist(fr, "yaw_belt", "yaw_belt distribution", "yawBeltDist.png")

  ## Split the dataset
  indices <- createDataPartition(fr$classe, p = 0.70, list = FALSE)
  frTrain <- fr[indices, ]
  frTest <- fr[- indices, ]

  ## Train a random forest algo
  trainCtrl <- trainControl(method = "oob")
  model <- train(classe ~ ., data = frTrain, method = "rf", trControl = trainCtrl, tuneLength = 5, ntree = 200, do.trace = DEBUG)
  if (0) {
    ## To make rapid technical tests
    model <- train(classe ~ ., data = frTrain, method = "rf", trControl = trainCtrl, tuneLength = 1, ntree = 3, do.trace = DEBUG)
  }
  if (DEBUG) {
    print(model)
  }

  ## Estimate the model on the test set, and be sure no important info is in the test
  frTest2 <- frTest[, - which(colnames(frTest) %in% c("classe"))]
  pred <- predict(model, newdata = frTest2)
  cm <- confusionMatrix(pred, frTest$classe)

  ## The confusion matrix on the test set
  if (DEBUG) {
    print(cm$table)
  }

  ## The overall accuracy with an intervalle
  if (DEBUG) {
    print(paste(cm$overall["Accuracy"], " [", cm$overall["AccuracyLower"], " - ", cm$overall["AccuracyUpper"], "]", sep = ""))
  }

  ## The sensitivity and specificity by class
  if (DEBUG) {
    print(cm$byClass[, c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value")])
  }

  ## Prediction on the final test
  pred <- predict(model, newdata = frFinalTest)
  if (DEBUG) {
    print(paste("final predictions : ", paste(as.character(pred), collapse = ","), collapse = "", sep = ""))
  }
  
  ## Save it to a file
  pml_write_files(as.character(pred))
  
  ## Save all important data structures to file
  save(frTrain, frTest, frFinalTest, model, cm, file = "data.rda")
}

## To load the file into a dataframe
loadFile <- function(file) {
  fr <- read.csv(file)
  fr <- fr[, -1]

  return(fr)
}

## To cleanup the data
cleanUp <- function(fr) {
  for (colName in c("kurtosis_roll_belt", "kurtosis_picth_belt", "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt", "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "kurtosis_roll_arm", "kurtosis_picth_arm", "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm", "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell", "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell", "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm", "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_yaw_forearm", "min_yaw_forearm", "amplitude_yaw_forearm")) {
    
    ## To check that non numeric values were only "" and "#DIV/0!"
    if (0) {
      print(findNonNumericValues(fr[, colName]))
    }
    indices <- findNonNumericValueIndices(fr[, colName])
    if (length(indices) > 0) {
      fr[indices, colName] <- NA
      fr[, colName] <- as.numeric(fr[, colName])
    }
  }

  ## Eliminate the variables that have more than 95% of NA (all variables have either 0 NA or more than 97% NA
  indicesToKeep <- c()
  for (i in seq(ncol(fr))) {
    values <- fr[, i]
    percentNA <- 100 * sum(is.na(values)) / length(values)
    if (percentNA < 95) {
      indicesToKeep <- c(indicesToKeep, i)
    }
  }
  if (DEBUG) {
    cat("Number of variables that was excluded : ", ncol(fr) - length(indicesToKeep), " out of ", ncol(fr), " variables\n", sep = "")
  }
  fr <- fr[, indicesToKeep]
  
  ## Those variables are very probably to eliminate
  if (nrow(fr) > 100) {
    save(fr, file = "initialData.rda")
  }
  fr <- fr[, - which(colnames(fr) %in% c("raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window", "user_name"))]
  if (DEBUG) {
    cat("Final number of predicting variables remaining : ", ncol(fr) - 2, "\n", sep = "")
  }
  
  return(fr)
}

## Find the non numeric values
findNonNumericValues <- function(vect) {
  normalIndices <- grep("^-?[\\d\\.]+$", vect, perl = TRUE)
  if (length(normalIndices) != length(vect)) {
    if (length(normalIndices) == 0) {
      abnormalValues <- as.character(vect)
    } else {
      abnormalValues <- as.character(vect[- normalIndices])
    }
    
    return(unique(abnormalValues))
  } else {
    return(NULL)
  }
}

## Find the non numeric values indices
findNonNumericValueIndices <- function(vect) {
  normalIndices <- grep("^-?[\\d\\.]+$", vect, perl = TRUE)
  abnormalIndices <- setdiff(seq(length(vect)), normalIndices)

  return(abnormalIndices)
}

## To plot an histogram
plotHist <- function(fr, colName, title, outputFile) {
  png(outputFile)
  hist(fr[, colName], breaks = 100, col = "skyblue", xlab = colName, main = title)
  graphics.off()
}

## The function given to write the files
pml_write_files <- function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

main()
