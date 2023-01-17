#' Evaluation Metrics Computation for Binary Classification
#' @param actual The actual data (usually obtained from the test data)
#' @param predicted Predictions from an arbitrary model
#' @export
#' @return Returns a hash of all evaluation metrics
#' @details
#' This functions computes various evaluation metrics of classification for a given
#' actual data and a prediction data, both of which should be binary class.
#' The function returns a hash containing the
#' following items: accuracy, precision, recall, F-score, F-beta score, confusion
#' matrix, and plot confusion matrix
#' @examples
#' actual <- insurance_test$outcome
#' glm_fit <- glm_fit_hmd(data = insurance_train,
#' target="outcome",
#' family="binomial")
#' h <- glm_predict_hmd(fit = glm_fit,
#' data = insurance_test,
#' target ="outcome",
#' threshold = 0.5, s=0)
#' predicted <- h$predictions
#' h <- eval_hmd(actual, predicted)
#' acc <- h$accuracy
#' acc

eval_hmd <- function(actual,
                    predicted){

  y_actual <- {{actual}}
  y_predicted <- {{predicted}}

  if (is.factor(y_actual)){
    y_actual <- as.numeric(y_actual)
    y_actual <- ifelse(y_actual==1,0,1)
  }

  if (is.factor(y_predicted)){
    y_predicted <- as.numeric(y_predicted)
    y_predicted <- ifelse(y_predicted==1,0,1)
  }

  if (!(is.matrix(y_actual))){
    if (!(is.numeric(y_actual))){
      stop("actual input argument should contain numbers.
         Please ensure actual's class is either numeric or factor")
    }
  }

  if (!(is.matrix(y_predicted))){
    if (!(is.numeric(y_predicted))){
      stop("predict input argument should contain numbers.
         Please ensure predict's class is either numeric or factor")
    }
  }

  if (!(y_actual |> unique() |> length() <= 2) | !(y_predicted |> unique() |> length() <= 2)){
    stop("Both actual and predicted should be binary. Ensure they contain only two classes.")
  }

  if (!(all(unique(y_predicted) %in% unique(y_actual))) & !(all(unique(y_actual) %in% unique(y_predicted)))){
    stop("predicted input argument cannot have any class outside of actual classes")
  }



  cm <- caret::confusionMatrix(data = as.factor(y_predicted),
                               reference = as.factor(y_actual),
                               dnn = c("Prediction", "Reference"))
  acc <- Metrics::accuracy(y_predicted, y_actual)
  prec <- Metrics::precision(y_predicted, y_actual)
  rec <- Metrics::recall(y_predicted, y_actual)
  f1 <- Metrics::f1(y_predicted, y_actual)
  fb <- Metrics::fbeta_score(y_predicted, y_actual)


  plt <- as.data.frame(cm$table)
  plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
  options(repr.plot.width=2, repr.plot.height=2)
  cm_plot <- ggplot2::ggplot(plt, ggplot2::aes( plt$Reference, plt$Prediction, fill=plt$Freq)) +
    ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label=plt$Freq)) +
    ggplot2::scale_fill_gradient(low="white", high="#009194") +
    ggplot2::labs(x = "Prediction", y = "Reference") +
    ggplot2::scale_x_discrete(labels=c("Class 0","Class 1")) +
    ggplot2::scale_y_discrete(labels=c("Class 1","Class 0")) +
    ggplot2::theme(text = ggplot2::element_text(size = 9), ggplot2::element_line(size =1), plot.margin = ggplot2::unit(c(1,1,1,1),"cm"))

  h <- hash::hash()
  h[["confusion_matrix"]] <- cm
  h[["confusion_matrix_plot"]] <- cm_plot
  h[["accuracy"]] <- acc
  h[["precision"]] <- prec
  h[["recall"]] <- rec
  h[["f1_score"]] <- f1
  h[["fbeta_score"]] <- fb
  return(h)
}
