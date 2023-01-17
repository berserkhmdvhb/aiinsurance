#' Plot lambda of CV.GLMNET function (cross-validated GLM model)
#' @param fit fit object from CV.GLMNET
#' @export
#' @return Returns plot of c
#' @details
#' Plots fit object fit against the log-lambda value and with each curve labeled:
#' @examples
#' fit <- glmnet_cv_fit_hmd(insurance_train, target = "outcome", family = "binomial")
#' glmnet_cv_plot_hmd(fit)

glmnet_cv_plot_hmd <- function(fit){
  if (!({{fit}} |> class() == "cv.glmnet")){
    stop("fit input argument should be obtained from cv.glmnet function or glmnet_cv_fit_hmd function")
  }
  return(plot({{fit}}, xvar = "lambda", label = TRUE))
}
