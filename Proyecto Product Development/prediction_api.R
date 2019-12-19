library(dplyr)
library(rpart)
library(plumber)
library(jsonify)
library (ROCR)
library(pROC)
#library(caret)

# Loading model
fit <- readRDS("winequalitymodel.rds")
fit2 <- readRDS("winequalitymodel_2.rds")

# Plumber Api methods

#* @apiTitle Predict Wine quality
#* @apiDescription wine quality prediction.

#' @param username User who makes the request.
#' @param model Model to use for testing input.
#' @param fixed_acidity:numeric most acids involved with wine or fixed or nonvolatile (do not evaporate readily)
#' @param volatile_acidity:numeric the amount of acetic acid in wine, which at too high of levels can lead to an unpleasant, vinegar taste
#' @param citric_acid:numeric found in small quantities, citric acid can add 'freshness' and flavor to wines
#' @param residual_sugar:numeric the amount of sugar remaining after fermentation stops, it's rare to find wines with less than 1 gram/liter and wines with greater than 45 grams/liter are considered sweet
#' @param chlorides:numeric the amount of salt in the wine
#' @param free_sulfur_dioxide:numeric the free form of SO2 exists in equilibrium between molecular SO2 (as a dissolved gas) and bisulfite ion; it prevents microbial growth and the oxidation of wine
#' @param total_sulfur_dioxide:numeric amount of free and bound forms of S02; in low concentrations, SO2 is mostly undetectable in wine, but at free SO2 concentrations over 50 ppm, SO2 becomes evident in the nose and taste of wine
#' @param density:numeric the density of water is close to that of water depending on the percent alcohol and sugar content
#' @param pH:numeric describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic); most wines are between 3-4 on the pH scale
#' @param sulphates:numeric a wine additive which can contribute to sulfur dioxide gas (S02) levels, wich acts as an antimicrobial and antioxidant
#' @param alcohol:numeric the percent alcohol content of the wine
#' @post /predict
#' @response 200 Returns the quality variable (based on sensory data, score between 0 and 10)

function(req, res, username = NA, model = NA, fixed_acidity = NA, volatile_acidity = NA, citric_acid = NA, residual_sugar = NA, chlorides = NA,
         free_sulfur_dioxide = NA, total_sulfur_dioxide = NA, density = NA, pH = NA, sulphates = NA, alcohol = NA) {
  
  errors <- FALSE
  
  # Creating features data frame
  features <- data_frame(fixed.acidity = as.numeric(fixed_acidity),
                         volatile.acidity = as.numeric(volatile_acidity),
                         citric.acid = as.numeric(citric_acid),
                         residual.sugar = as.numeric(residual_sugar),
                         chlorides = as.numeric(chlorides),
                         free.sulfur.dioxide= as.numeric(free_sulfur_dioxide),
                         total.sulfur.dioxide = as.numeric(total_sulfur_dioxide),
                         density = as.numeric(density),
                         pH = as.numeric(pH),
                         sulphates = as.numeric(sulphates),
                         alcohol = as.numeric(alcohol))
  
  # Validations
  if (any(is.na(features))) {
    res$status <- 400
    res$body <- "Feature parameters have to be numeric or integers"
    errors <- TRUE
  }
  
  # Validations
  if (is.na(model))
  {
    res$status <- 400
    res$body <- "Parameter model is required"
    errors <- TRUE
  }
  
  if (!is.integer(model)) {
    res$status <- 400
    res$body <- "Parameter model must be an integer value"
    errors <- TRUE
  }
  
  if (!errors) {
    # Executing prediction
    if (model == 1)
    {
      # model 1
      out <- predict(fit, features, type="class")
      res$body <- out
      
    } else if (model == 2) {
      # model 2
      out <- predict(fit2, features, type="class")
      res$body <- out
    } else {
      res$status <- 400
      res$body <- "Model does not exist"
    }
  }
}


#* @apiTitle Load test data
#* @apiDescription Load test data to model.

#' @param username User who makes the request.
#' @param model Model to use for testing input.
#' @param batchdata Data to be predicted
#' @post /batchpredict
#' @response 200 Returns predictions

function(req, res, username = NA, model = NA, batchdata = NA) {
  
  errors <- FALSE
  
  if(any(is.na(batchdata)))
  {
    res$status <- 400
    res$body <- "All batchdata parameters have to be numeric or integers"
    errors <- TRUE
  }
  
  if (is.na(model))
  {
    res$status <- 400
    res$body <- "Parameter model is required"
    errors <- TRUE
  }
  
  if (!is.integer(model)) {
    res$status <- 400
    res$body <- "Parameter model must be an integer value"
    errors <- TRUE
  }
  
  colnames(batchdata)[which(names(batchdata) == "fixed_acidity")] <- "fixed.acidity"
  colnames(batchdata)[which(names(batchdata) == "volatile_acidity")] <- "volatile.acidity"
  colnames(batchdata)[which(names(batchdata) == "citric_acid")] <- "citric.acid"
  colnames(batchdata)[which(names(batchdata) == "residual_sugar")] <- "residual.sugar"
  colnames(batchdata)[which(names(batchdata) == "free_sulfur_dioxide")] <- "free.sulfur.dioxide"
  colnames(batchdata)[which(names(batchdata) == "total_sulfur_dioxide")] <- "total.sulfur.dioxide"
  
  if (!errors) {
    # Executing prediction
    if (model == 1)
    {
      # model 1
      out <- predict(fit, batchdata, type="class")
      res$body <- out
      
    } else if (model == 2) {
      # model 2
      out <- predict(fit2, batchdata, type="class")
      res$body <- out
    } else {
      res$status <- 400
      res$body <- "Model does not exist"
    }
  }
}



#* @apiTitle Model performance
#* @apiDescription Test model performance and return performance metrics

#' @param username User who makes the request.
#' @param model Model to use for testing input.
#' @param testdata Data to be tested for evaluating model performance
#' @post /modelperformance
#' @response 200 Returns performance metrics

function(req, res, username = NA, model = NA, testdata = NA) {
  
  errors <- FALSE
  
  if(any(is.na(testdata)))
  {
    res$status <- 400
    res$body <- "All test data parameters have to be numeric or integers"
    errors <- TRUE
  }
  
  if (is.na(model))
  {
    res$status <- 400
    res$body <- "Parameter model is required"
    errors <- TRUE
  }
  
  if (!is.integer(model)) {
    res$status <- 400
    res$body <- "Parameter model must be an integer value"
    errors <- TRUE
  }
  
  colnames(testdata)[which(names(testdata) == "fixed_acidity")] <- "fixed.acidity"
  colnames(testdata)[which(names(testdata) == "volatile_acidity")] <- "volatile.acidity"
  colnames(testdata)[which(names(testdata) == "citric_acid")] <- "citric.acid"
  colnames(testdata)[which(names(testdata) == "residual_sugar")] <- "residual.sugar"
  colnames(testdata)[which(names(testdata) == "free_sulfur_dioxide")] <- "free.sulfur.dioxide"
  colnames(testdata)[which(names(testdata) == "total_sulfur_dioxide")] <- "total.sulfur.dioxide"
  
  if (!errors) {
    # Executing prediction
    if (model == 1)
    {
      # model 1
      predictions <- predict(fit, testdata, type="class")
      
    } else if (model == 2) {
      # model 2
      predictions <- predict(fit2, testdata, type="class")
    } else {
      res$status <- 400
      res$body <- "Model does not exist"
      errors <- TRUE
    }
    
    
    if (!errors) {
      predictions = as.numeric(as.character(predictions))
      
      # confusion matrix
      conf.matrix <- table(testdata$quality, predictions)
      rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = "_")
      colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = "_")
      
      rocobj <- multiclass.roc(testdata$quality, predictions)
      auc <- rocobj$auc
      accuracy <- length(which(predictions == testdata$quality)) / length(testdata$quality)
      
      # Final metrics
      metrics <- list(confusion_matrix = as.data.frame(conf.matrix), auc = auc, accuracy = accuracy)
      res$body <- as.character(metrics)
    }
  }
}


