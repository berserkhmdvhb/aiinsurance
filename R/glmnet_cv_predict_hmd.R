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
#' @examples
#' fit <- glmnet_cv_fit_hmd(insurance_train,
#' target = "outcome",
#' family = "binomial")
#' glmnet_cv_predict_hmd(fit,
#' data = insurance_test,
#' target = "outcome",
#' lchoice = "min",
#' threshold = 0.5)
#' h <- glmnet_cv_predict_hmd(fit, data = insurance_test,
#' target = "outcome",
#' lchoice = "min")
#' predictions <- h$predictions
#' predictions

glmnet_cv_predict_hmd <- function(fit,
                                  data = aiinsurance::insurance_test,
                                  target = "outcome",
                                  lchoice="min",
                                  threshold = 0.5
                          ){
  if (!(is.data.frame({{data}}))){
    stop("data input argument should be a dataframe.")
  }
  if (!({{fit}} |> class() == "cv.glmnet")){
    stop("fit input argument should be obtained from cv.glmnet function or glmnet_cv_fit_hmd function")
  }
  if (!({{lchoice}} %in% c("min", "1se"))){
    stop("lchoice input argument can be min or 1se.")
  }
  if (!(is.character({{target}}))){
    stop("target input argument should be of type character ")
  }
  if (!(is.numeric({{threshold}}) == "TRUE")){
    stop("threshould input argument should be a number.")
  }
  if (!(dplyr::between({{threshold}}, 0, 1) == "TRUE")){
    stop("threshould input argument should be between 0 and 1")
  }
  if (!(is.character({{target}}))){
    stop("target input argument should be of type character ")
  }
  if (!({{target}} %in% names({{data}}))){
    stop("target input argument should be contained in dataframe from data input argument.")
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
