#' Predict data with results from CV.GLMNET model
#' @param fit fit object from a cv.glmnet model
#' @param data An arbitrary dataframe
#' @param target Target column in the data
#' @param lchoice how to choose lambda model. Choices are "min", "1se".
#' @param threshold Cutoff point of probability of class 1, to classify the inputs
#' with the probability above threshold as 1.
#' @export
#' @return Returns a hash of predictions, prediction probabilities, and coefficients
#' @details
#' This functions allows the user to predict a given dataset using fit object
#' obtained from the cv.glmnet function

glmnet_cv_predict_hmd <- function(fit,
                                  data = aiinsurance::insurance_test,
                                  target = "outcome",
                                  lchoice="min",
                                  threshold = 0.5
                          ){
  if (!(is.data.frame({{data}}))){
    stop("The data input argument should be a dataframe.")
  }

  features_names=names({{data}})[names({{data}}) != {{target}}]

  # make a copy of data with different pointer in memory
  df <- data.frame({{data}})
  # extract feature names either from input or dataframe

  features <- data.matrix(df[features_names])
  target_col <- as.numeric(unlist(df[{{target}}]))

  lambda <- paste("lambda.",{{lchoice}},sep = "")
  coef <- coef({{fit}}, s=lambda)

  predict_proba <- stats::predict({{fit}}, type="response", features, s=lambda)
  predict <- ifelse(predict_proba > {{threshold}}, 1, 0)

  h <- hash::hash()
  h <- hash::hash()
  h[["coef"]] <- coef
  h[["predict_proba"]] <- predict_proba
  h[["predictions"]] <- predict
  return(h)
}
