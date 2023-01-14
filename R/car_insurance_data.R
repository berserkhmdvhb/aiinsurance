#' @title Car Insurance Data
#' @description
#' This dataset includes annual car insurance data shared by a company. Most of
#' the data is real and part of it is artificially generated. The target column is
#' `outcome`. This binary column shows if the insurance request was rejected or accepted.
#'
#' @docType data
#' @usage data(car_insurance_data)
#' @format A data frame with 10000 rows and 19 columns:
#' \describe{
#'   \item{ID}{Unique ID of the policyholder.}
#'   \item{AGE}{Age of the policyholder, categorize: 26-39, 40-64, other.}
#'   \item{GENDER}{Gender of the policyholder, categorized as: male, female.}
#'   \item{RACE}{Rage of policyholder, categorized as: major, minor.}
#'   \item{DRIVING_EXPERIENCE}{Duration of the policyholder's experience in driving, categorized as:
#'   0-9y, 10-19y, other.}
#'   \item{EDUCATION}{Education level of the policyholder, categorized as: high school, university.}
#'   \item{INCOME}{Income cass of the policyholder, categorized as: upper class, middle class, other.}
#'   \item{CREDIT_SCORE}{Credit score of policyholder.}
#'   \item{VEHICLE_OWNERSHIP}{Whether the policyholder owns the vehicle or not.}
#'   \item{VEHICLE_YEAR}{The year vehicle was built, categorized as: before 2015, after 2015.}
#'   \item{MARRIED}{Whether the policyholder is married or not.}
#'   \item{CHILDREN}{Whether the policyholder has children or not.}
#'   \item{POSTAL_CODE}{Postal code of the policyholder.}
#'   \item{VEHICLE_TYPE}{The type of the vehicle of the policyholder, categorized as: sedan, sports car.}
#'   \item{ANNUAL_MILEAGE}{Annual mileage of the vehicle of the policyholder.}
#'   \item{SPEEDING_VIOLATIONS}{Number of speeing violations recorded for policyholder}
#'   \item{DUIS}{Number of times policyholder did driving under the influence (DUI).}
#'   \item{PAST_ACCIDENTS}{Number of policyholder's past driving accidents.}
#'   \item{OUTCOME}{Indicates 1 if a customer has claimed his/her loan else 0.}
#' }
#' @source <https://www.kaggle.com/datasets/sagnik1511/car-insurance-data>
"car_insurance_data"
