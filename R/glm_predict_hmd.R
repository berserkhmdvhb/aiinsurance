#' Predict data with results using either GLM fit object or GLMNET fit ojbect
#' @param fit fit object from a glm model
#' @param data An arbitrary dataframe
#' @param target An arbitrary dataframe
#' "response" gives the fitted probabilities. "class" produces the class label
#' corresponding to the maximum probability.
#' @param target Target column in the data
#' @param threshold Cutoff point of probability of class 1, to classify the inputs
#' with the probability above threshold as 1.
#' @param s "s" controls the amount of shrinkage or regularization applied to the model.
#' It is a value between 0 and 1, where 0 corresponds to Ridge regression (no variable selection) and 1 corresponds to Lasso regression (variable selection).
#' @export
#' @return Returns fit object of glm function
#' @details
#' This functions allows the user to predict the results obtained from either
#' GLM model or GLMNET function (Elastic-net regression)
#'

glm_predict_hmd <- function(fit,
                            data = aiinsurance::insurance_test,
                            target ="outcome",
                            threshold = 0.5,
                            s=0.5
                          ){
  if(nrow({{data}}) == 0) {
    warning("The returned data frame is empty.")
  }
  features_names=names({{data}})[names({{data}}) != {{target}}]

  # make a copy of data with different pointer in memory
  df <- data.frame({{data}})
  # extract feature names either from input or dataframe

  features <- df[, colnames(df)[colnames(df) != {{target}}]]
  features_names <- names(df)[names(df) != {{target}}]
  coef <- coef({{fit}})

  if (setequal(class({{fit}}), c("glm", "lm"))){
    predict_proba <- stats::predict({{fit}}, features, type="response")
  }
  else if (setequal(class({{fit}}), c("lognet", "glmnet"))){
    format <- stats::as.formula(paste({{target}}, "~",
                                      paste(features_names, collapse = "+")))
    X <- stats::model.matrix(format, data=df)
    y <- data.matrix((df[{{target}}]))
    predict_proba <- c(stats::predict({{fit}}, X, type="response", s={{s}}))
  }
  else{
    stop("The fit input should be from either glm or glmnet function")
  }

  predict <- ifelse(predict_proba > {{threshold}}, 1, 0)
  h <- hash::hash()
  h[["coef"]] <- coef
  h[["predict_proba"]] <- predict_proba
  h[["predictions"]] <- predict
  return(h)
}
