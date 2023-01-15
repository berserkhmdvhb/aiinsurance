#' Predict data with results using GLM fit object
#' @param fit fit object from a glm model
#' @param data An arbitrary dataframe
#' @param target An arbitrary dataframe
#' "response" gives the fitted probabilities. "class" produces the class label
#' corresponding to the maximum probability.
#' @param target Target column in the data
#' @param threshold Cutoff point of probability of class 1, to classify the inputs
#' with the probability above threshold as 1.
#' @export
#' @return Returns fit object of glm function
#' @details
#' This functions allows the user to predict the results obtained from GLM model

glm_predict_hmd <- function(fit,
                               data = aiinsurance::insurance_test,
                               target ="outcome",
                               threshold = 0.5
                          ){
  if(nrow({{data}}) == 0) {
    warning("The returned data frame is empty.")
  }
  features_names=names({{data}})[names({{data}}) != {{target}}]

  # make a copy of data with different pointer in memory
  df <- data.frame({{data}})
  # extract feature names either from input or dataframe

  features <- df[, colnames(df)[colnames(df) != {{target}}]]
  coef <- coef({{fit}})
  predict_proba <- stats::predict({{fit}}, features, type="response")
  predict <- ifelse(predict_proba > {{threshold}}, 1, 0)
  h <- hash::hash()
  h[["coef"]] <- coef
  h[["predict_proba"]] <- predict_proba
  h[["predictions"]] <- predict
  return(h)
}
