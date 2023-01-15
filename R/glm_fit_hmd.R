#' GLM model
#' @param data An arbitrary dataframe
#' except the target
#' @param target The target variable aimed for prediction
#' @param family specify family of distribution.
#' @export
#' @return Returns fit object of glm function
#' @details
#' This functions allows the user to perform generalized linear model (GLM) on a given dataframe by specifying feature names (response variables),
#' target variable, family of distribution, and the dataset.

glm_fit_hmd <- function(data=aiinsurance::insurance_train,
                              target="outcome",
                              family="binomial"){
  # ensure dataframe is not empty

  if(nrow({{data}}) == 0) {
    warning("The returned data frame is empty.")
  }
  # make a copy of data with different pointer in memory
  df <- data.frame({{data}})
  features_names <- names(df)[names(df) != {{target}}]
  #features <- df[, colnames(df)[colnames(df) != {{target}}]]
  # extract feature names either from input or dataframe
  glm_format <- stats::as.formula(paste({{target}}, "~",
                                        paste(features_names, collapse = "+"),
                                        sep = ""
                                        ))
  fit <- stats::glm(glm_format,
                    data=df,
                    family={{family}}
                   )
  return(fit)
}
