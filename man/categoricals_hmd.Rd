% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/categoricals_hmd.R
\name{categoricals_hmd}
\alias{categoricals_hmd}
\title{Treat categorical columns}
\usage{
categoricals_hmd(data = aiinsurance::car_insurance_data)
}
\arguments{
\item{data}{An arbitrary dataframe}
}
\value{
Returns dataframe with its categorical columns factorised
}
\description{
Treat categorical columns
}
\details{
This functions treats the categorical columns by first detecting them in a given
dataframe and then convert those including characters to factors. To detect
categorical columns, the function recognize a column as categorical if either
its class is "character", or the number of unique values in
that column has is less than 5.
}
\examples{
df <- categoricals_hmd(data = car_insurance_data)
dplyr::glimpse(df)
}
