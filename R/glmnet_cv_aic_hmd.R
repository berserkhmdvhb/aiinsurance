#' Extract AIC from CV.GLMNET function (cross-validated GLM model)
#' @param fit Fit object from a cv.glmnet model
#' @param lchoice How to choose lambda model. Choices are "min", "1se".
#' @export
#' @return AIC and BIC from a glm fit object
#' @details
#' This function allows the user to extract AIC and BIC from a cv.glmnet fit object.
#' @examples
#' fit_glmnet_cv <- glmnet_cv_fit_hmd(insurance_train, target = "outcome", family = "binomial")
#' glmnet_cv_aic_hmd(fit_glmnet_cv, lchoice = "min")

glmnet_cv_aic_hmd <- function(fit,
                              lchoice="min"
){

  if (!(is.character({{lchoice}}))){
    stop("lchoice input argument should be of type character.")
  }
  if (!({{lchoice}} %in% c("min", "1se"))){
    stop("lchoice input argument can be min or 1se.")
  }
  if (!({{fit}} |> class() == "cv.glmnet")){
    stop("fit input argument should be obtained from cv.glmnet function or glmnet_cv_fit_hmd function")
  }
  lambda <- paste("lambda.", {{lchoice}},sep = "")

  whlm <- which({{fit}}$lambda == {{fit}}[[lambda]])
  with({{fit}}$glmnet.fit,
       {
         tLL <- nulldev - nulldev * (1 - dev.ratio)[whlm]
         k <- df[whlm]
         n <- nobs
         return(list('AICc' = - tLL + 2 * k + 2 * k * (k + 1) / (n - k - 1),
                     'BIC' = log(n) * k - tLL))
       })
}
