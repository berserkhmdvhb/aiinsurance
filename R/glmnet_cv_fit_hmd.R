#' Cross-validated GLM model
#' @param data An arbitrary dataframe
#' except the target
#' @param target The target variable aimed for prediction
#' @param family specify family of distribution.
#' @export
#' @return Returns fit object of glmnet function
#' @details
#' This functions allows the user to perform cross-validated elastic-net
#' regression on a given dataframe by specifying feature names (response variables),
#' target variable, family of distribution, and the dataset.
#' @examples
#' fit <- glmnet_cv_fit_hmd(data = insurance_train,
#' target = "outcome",
#' family = "binomial")
#' summary(fit)

glmnet_cv_fit_hmd <- function(data=aiinsurance::insurance_train,
                          target="outcome",
                          family="binomial"){
  # ensure dataframe is not empty
  if (!(is.data.frame({{data}}))){
    stop("data input argument should be a dataframe.")
  }
  if (!(is.character({{target}}))){
    stop("target input argument should be of type character ")
  }
  if (!(is.character({{family}}))){
    stop("family input argument should be of type character ")
  }
  if (!({{family}} %in% c("binomial",
                          "gaussian",
                          "Gamma",
                          "inverse.gaussian",
                          "poisson",
                          "quasi",
                          "quasibinomial",
                          "quasipoisson"))){
    stop("family input argument should one of the ones defined, see the documentation to choose.")
  }
  if (!({{target}} %in% names({{data}}))){
    stop("target input argument should be contained in dataframe from data input argument.")
  }
  features_names <- names({{data}})[names({{data}}) != {{target}}]

  # make a copy of data with different pointer in memory
  df <- data.frame({{data}})
  # extract feature names either from input or dataframe

  features <- data.matrix(df[features_names])
  target_col <- as.numeric(unlist(df[{{target}}]))
  fit <- glmnet::cv.glmnet(features,
                           target_col,
                           family={{family}}
                           )
  return(fit)
}
