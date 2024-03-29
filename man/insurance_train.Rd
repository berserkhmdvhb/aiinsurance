% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insurance_train.R
\docType{data}
\name{insurance_train}
\alias{insurance_train}
\title{Processed training dataset from car insurance data}
\format{
A data frame with 10000 rows and 19 columns:
\describe{
  \item{age.16.25}{Age of the policyholder}
  \item{age.26.39}{Age of the policyholder}
  \item{age.40.64}{Age of the policyholder}
  \item{age.65.}{Age of the policyholder}
  \item{gender.female}{Gender of the policyholder}
  \item{gender.male}{Gender of the policyholder}
  \item{race.majority}{Rage of policyholder}
  \item{race.minority}{Rage of policyholder}
  \item{driving_experience.0.9y}{Duration of the policyholder's experience in driving}
  \item{driving_experience.10.19y}{Duration of the policyholder's experience in driving}
  \item{driving_experience.20.29y}{Duration of the policyholder's experience in driving}
  \item{driving_experience.30y.}{Duration of the policyholder's experience in driving}
  \item{education.high.school}{Education level of the policyholder}
  \item{education.none}{Education level of the policyholder}
  \item{education.university}{Education level of the policyholder}
  \item{income.middle.class}{Income cass of the policyholder}
  \item{income.poverty}{Income cass of the policyholder}
  \item{income.upper.class}{Income cass of the policyholder}
  \item{income.working.class}{Income cass of the policyholder}
  \item{credit_score}{Credit score of policyholder}
  \item{vehicle_ownership}{Whether the policyholder owns the vehicle or not.}
  \item{vehicle_year.after.2015}{The year vehicle was built}
  \item{vehicle_year.before.2015}{The year vehicle was built}
  \item{married}{Whether the policyholder is married or not}
  \item{children}{Whether the policyholder has children or not}
  \item{postal_code}{Postal code of the policyholder}
  \item{annual_mileage}{Annual mileage of the vehicle of the policyholder.}
  \item{vehicle_type.sedan}{The type of the vehicle of the policyholder.}
  \item{vehicle_type.sports.car}{The type of the vehicle of the policyholder}
  \item{speeding_violations}{Number of speeing violations recorded for policyholder}
  \item{duis}{Number of times policyholder did driving under the influence (DUI)}
  \item{past_accidents}{Number of policyholder's past driving accidents}
  \item{outcome}{Indicates 1 if a customer has claimed his/her loan else 0}
}
}
\usage{
data(insurance_train)
}
\description{
This dataset is the training dataset obtained by processing the car insurance data.
The processing steps are described in the report as well as documentation of function preprocess_hmd.
}
\keyword{datasets}
