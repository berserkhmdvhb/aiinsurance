#' Imputes missing values with median
#' @param data An arbitrary dataframe
#' @export
#' @return Returns dataframe with each missing value is replaced by the median of the
#' column in which the missing value exists.
#' @details
#' This function receives a dataframe, finds columns containing NA values, and then
#' replaces NA values of each found column with median of that column.
#' @examples
#' df <- impute_median_hmd(data = car_insurance_data)
#' dplyr::glimpse(df)

impute_median_hmd <- function(data=aiinsurance::car_insurance_data){
  # ensure dataframe is not empy
  if (!(is.data.frame({{data}}))){
    stop("data input argument should be a dataframe.")
  }
  df <- data.frame({{data}})
  cols_list = which(colSums(is.na(df))>0)
  if (length({{cols_list}}) == 0)
  {
    warning("No column has NA value. No columns will be modified.")
    return(df)
  }
  for (col in {{cols_list}}){
    df[, col][is.na(df[, col])] <- stats::median(df[, col], na.rm = T)
  }
  return(df)
}

