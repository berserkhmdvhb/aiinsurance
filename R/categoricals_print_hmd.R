#' Print nature of categorical columns
#' @param data An arbitrary dataframe
#' @export
#' @details
#' This functions first detects which columns are categorical and then prints whether
#' the detected columns contain numbers or characters. To detect categorical columns,
#' the function recognizes a column as categorical if either its class is "character",
#' or the number of unique values in that column has is less than 5.
#' @examples
#' categoricals_print_hmd(data = car_insurance_data)

categoricals_print_hmd <- function(data=aiinsurance::car_insurance_data){
  # ensure dataframe is not empy
  if (!(is.data.frame({{data}}))){
    stop("data input argument should be a dataframe.")
  }

  # make a copy of data with different pointer in memory
  df <- data.frame({{data}})
  char_cols = list()
  num_cols = list()
  cat_cols = list()
  for (col in colnames(df)){
    if (is.character(df[[col]])){
      char_cols <- append(char_cols, col)
    }
    else if (length(unique(df[[col]])) < 5){
      num_cols <- append(num_cols, col)
    }
  }
  cat_cols = append(char_cols, num_cols)
  char_cols <- unlist(char_cols, use.names=FALSE)
  num_cols <- unlist(num_cols, use.names=FALSE)
  cat_cols <- unlist(cat_cols, use.names=FALSE)

  print("List of categorical columns containing characters: ")
  print(char_cols)
  print("List of categorical columns containing numbers: ")
  print(num_cols)
}
