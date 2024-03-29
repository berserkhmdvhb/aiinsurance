% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glm_predict_hmd.R
\name{glm_predict_hmd}
\alias{glm_predict_hmd}
\title{Predict data with results using either GLM fit object or GLMNET fit ojbect}
\usage{
glm_predict_hmd(
  fit,
  data = aiinsurance::insurance_test,
  target = "outcome",
  threshold = 0.5,
  s = 0.5
)
}
\arguments{
\item{fit}{fit object from a glm or glmnet model}

\item{data}{An arbitrary dataframe}

\item{target}{Target column in the data}

\item{threshold}{Cutoff point of probability of class 1, to classify the inputs
with the probability above threshold as 1.}

\item{s}{"s" controls the amount of shrinkage or regularization applied to the model.
It is a value between 0 and 1, where 0 corresponds to Ridge regression (no variable selection) and 1 corresponds to Lasso regression (variable selection).}
}
\value{
Returns a hash of predictions, prediction probabilities, and coefficients
}
\description{
Predict data with results using either GLM fit object or GLMNET fit ojbect
}
\details{
This functions allows the user to predict the results obtained from either
GLM model or GLMNET function (Elastic-net regression)
}
\examples{
fit_glm <- glm_fit_hmd(data = insurance_test,
target = "outcome",
family="binomial")
h <- glm_predict_hmd(fit_glm,
insurance_test,
target = "outcome",
threshold = 0.5,
s = 0.5)
predictions <- h$predictions
predictions
}
