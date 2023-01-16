#' Random Forest Model Fitting
#' @param data An arbitrary dataframe
#' @param target The target variable aimed for prediction
#' @param ntree Number of trees to grow. This should not be set to too small a '
#' umber, to ensure that every input row gets predicted at least a few times.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
#' @param proximity Should proximity measure among the rows be calculated?
#' @param importance Should importance of predictors be assessed?
#' @export
#' @return Returns fit object of randomForest function
#' @details
#' This functions allows the user to fit a random forest on a given dataframe
#' and to choose the hyperparameters for it.

rf_fit_hmd <- function(data=aiinsurance::insurance_train,
                           target="outcome",
                           ntree=300,
                           mtry=10,
                           proximity=TRUE,
                           importance=TRUE
                   ){
  # ensure dataframe is not empty

  if (!(is.data.frame({{data}}))){
    stop("data input argument should be a dataframe.")
  }
  if (!(is.numeric({{ntree}}))){
    stop("ntree input argument should be a number")
  }
  if (!(is.numeric({{mtry}}))){
    stop("mtry input argument should be a number")
  }
  if (!(all.equal({{ntree}}, as.integer({{ntree}})))){
    stop("ntree input argument should be an integer")
  }
  if (!(all.equal({{mtry}}, as.integer({{mtry}})))){
    stop("mtry input argument should be an integer")
  }
  if (!(is.logical({{proximity}}))){
    stop("proximity input argument should be boolean (TRUE or FALSE")
  }
  if (!(is.logical({{importance}}))){
    stop("importance input argument should be boolean (TRUE or FALSE")
  }
  if (!(is.character({{target}}))){
    stop("target input argument should be of type character ")
  }
  if (!({{target}} %in% names({{data}}))){
    stop("target input argument should be contained in dataframe from data input argument.")
  }

  # make a copy of data with different pointer in memory
  df <- data.frame({{data}})

  X_train <- df[, colnames(df)[colnames(df) != {{target}}]]
  y_train <- as.factor(df[[{{target}}]])

  fit <- randomForest::randomForest(x = X_train,
                                   y = y_train,
                                   ntree = {{ntree}},
                                   mtry = {{mtry}},
                                   proximity = {{proximity}},
                                   importance = {{importance}}
                                   )

  return(fit)
}
