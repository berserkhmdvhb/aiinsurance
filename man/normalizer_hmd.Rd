% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalizer_hmd.R
\name{normalizer_hmd}
\alias{normalizer_hmd}
\title{Normalize Train and Test Data}
\usage{
normalizer_hmd(
  train = aiinsurance::insurance_train,
  test = aiinsurance::insurance_test,
  method = "minmax"
)
}
\arguments{
\item{train}{Training Data}

\item{test}{Test Data}

\item{method}{If set to "std", then standard scaler is used, and if
set to "minmax", minmax is applied. Default is "minmax".}
}
\value{
Returns normalized dataframe
}
\description{
Normalize Train and Test Data
}
\details{
This functions allows the user to normalize given train and test data based on the method
user prefers. The methods available are "minmax", which transforms features by
scaling each feature to a given range, and "std", the standard scaler, which
standardizes features by removing the mean and scaling to unit variance.
}
\examples{
normalizer_hmd(train = insurance_train, test = insurance_test, method="minmax")
}
