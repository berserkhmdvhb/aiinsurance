#' Normalize Train and Test Data
#' @param train Training Data
#' @param test Test Data
#' @param method If set to "std", then standard scaler is used, and if
#' set to "minmax", minmax is applied. Default is "minmax".
#' @export
#' @return Returns normalized dataframe
#' @details
#' This functions allows the user to normalize given train and test data based on the method
#' user prefers. The methods available are "minmax", which transforms features by
#' scaling each feature to a given range, and "std", the standard scaler, which
#' standardizes features by removing the mean and scaling to unit variance.
#' @examples
#' normalizer_hmd(train = insurance_train, test = insurance_test, method="minmax")

normalizer_hmd <- function(train=aiinsurance::insurance_train,
                           test=aiinsurance::insurance_test,
                           method="minmax"){
  # ensure dataframe is not empty
  if (!(is.data.frame({{train}}))){
    stop("data input argument should be a dataframe.")
  }
  if (!(is.data.frame({{test}}))){
    stop("data input argument should be a dataframe.")
  }
  if (!({{method}} %in% c("minmax", "std"))){
    stop("method should be either minmax or std.")
  }
  # make a copy of data with different pointer in memory
  df_train <- data.frame({{train}})
  df_test <- data.frame({{test}})
  train_numeric <- dplyr::select_if(df_train, is.numeric)
  test_numeric <- dplyr::select_if(df_test, is.numeric)

  if(tolower({{method}}) == "minmax"){
    method <- c("range")
  }
  else if (tolower({{method}}) == "std"){
    method <- c("center", "scale")
  }
  # normalizer

  process <- caret::preProcess(train_numeric, method=c("range"))
  train_norm <- stats::predict(process, train_numeric)
  test_norm <- stats::predict(process, test_numeric)
  for (col in names(train_numeric)){
    if (col == "id"){
      next
    }
    #if (all.equal(df_train[[col]], as.integer(df_train[[col]])) == TRUE)
    if (is.integer(df_train[[col]]== TRUE))
    {
      next
    }
    df_train[col] <- train_norm[col]
    df_test[col] <- test_norm[col]
  }
  h <- hash::hash()
  h[["train_norm"]] <- df_train
  h[["test_norm"]] <- df_test
  return(h)
}

