#' CV GLMNET model
#' @param data An arbitrary dataframe
#' except the target
#' @param target The target variable aimed for prediction
#' @param family specify family of distribution.
#' @export
#' @return Returns fit object of glmnet function
#' @details
#' This functions allows the user to perform elastic-net
#' regression on a given dataframe by specifying feature names (response variables),
#' target variable, family of distribution, and the dataset.
#' @examples
#' fit <- glmnet_fit_hmd(data = insurance_train, target = "outcome", family = "binomial")
#' summary(fit)

glmnet_fit_hmd <- function(data=aiinsurance::insurance_train,
                           target="outcome",
                           family="binomial"){
  # ensure dataframe is not empty

  # ensure dataframe is not empy
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
  # make a copy of data with different pointer in memory
  df <- data.frame({{data}})
  features <- df[, colnames(df)[colnames(df) != {{target}}]]
  features_names <- names(df)[names(df) != {{target}}]
  format <- stats::as.formula(paste({{target}}, "~",
                                    paste(features_names, collapse = "+")))
  X <- stats::model.matrix(format, data=df)
  y <- data.matrix((df[{{target}}]))
  fit <- glmnet::glmnet(X, y, family={{family}})
  return(fit)
}
