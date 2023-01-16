#' Predict data with results from random forest model
#' @param fit fit object from a randomf forest model either from the randomForest
#' package)
#' @param data An arbitrary dataframe
#' @param target Target column in the data
#' @export
#' @return A hash containing predictions (both factorized and numerical), and prediction probabilities
#' @details
#' This functions allows the user to predict a given dataset using a randomForest fit object

rf_predict_hmd <- function(fit,
                           data = aiinsurance::insurance_test,
                           target = "outcome"

                           ){
  if (!(is.data.frame({{data}}))){
    stop("The data input argument should be a dataframe.")
  }

  df <- data.frame({{data}})

  X_test <- df[, colnames(df)[colnames(df) != {{target}}]]
  y_test <- as.factor(df[[{{target}}]])

  predict_rf <- stats::predict({{fit}}, X_test)
  pred_proba_rf <- stats::predict({{fit}}, X_test, type="prob")
  pred_proba_rf_list <- pred_proba_rf |> as.list() |> utils::tail(nrow(df)) |> unlist()
  predict_rf_num <- predict_rf |> as.double()
  predict_rf_num <- ifelse(predict_rf_num > 1, 1, 0)
  h <- hash::hash()
  h[["predict_proba"]] <- pred_proba_rf_list
  h[["predictions"]] <- predict_rf
  h[["predictions_num"]] <- predict_rf_num
  return(h)
}
