% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval_hmd.R
\name{eval_hmd}
\alias{eval_hmd}
\title{Evaluation Metrics Computation for Binary Classification}
\usage{
eval_hmd(actual, predicted)
}
\arguments{
\item{actual}{The actual data (usually obtained from the test data)}

\item{predicted}{Predictions from an arbitrary model}
}
\value{
Returns a hash of all evaluation metrics
}
\description{
Evaluation Metrics Computation for Binary Classification
}
\details{
This functions computes various evaluation metrics of classification for a given
actual data and a prediction data, both of which should be binary class.
The function returns a hash containing the
following items: accuracy, precision, recall, F-score, F-beta score, confusion
matrix, and plot confusion matrix
}
\examples{
actual <- insurance_test$outcome
glm_fit <- glm_fit_hmd(data = insurance_train,
target="outcome",
family="binomial")
h <- glm_predict_hmd(fit = glm_fit,
data = insurance_test,
target ="outcome",
threshold = 0.5, s=0)
predicted <- h$predictions
h <- eval_hmd(actual, predicted)
acc <- h$accuracy
acc
}
