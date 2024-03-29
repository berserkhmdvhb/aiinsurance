library(janitor)
library(stats)
library(hash)
library(caret)
library(glmnet)
library(car)
library(Metrics)
library(dplyr)
library(ggplot2)
library(pROC)
library(randomForest)
library(aiinsurance)
library(cachem)


appDir <- system.file("plot_app", package = "aiinsurance")
cacheDir <- paste(appDir,"/app-cache",sep="")
shinyOptions(cache = cachem::cache_disk(cacheDir,
                                        max_size = 2048 * 2048^2,
                                        max_age = Inf))
###
get_data_train <- function(){
  aiinsurance::insurance_train
}

get_data_test <- function(){
  aiinsurance::insurance_test
}

get_data_actual <- function(){
  aiinsurance::insurance_test$outcome
}

get_data_actual <- function(){
  aiinsurance::insurance_test$outcome
}

get_pred_proba <- function(h)
{
  h$predict_proba
}


roc_obj_cal <- function(actual, pred_proba){
  pROC::roc(actual ~ pred_proba, print.auc=FALSE)
}


plot_roc_curve <- function(roc_obj){
  pROC::ggroc(roc_obj)
}
###

shinyOptions(cache = cachem::cache_disk("./app-cache",
                                        max_age = Inf,
                                        max_size = 2048 * 2048^2))
