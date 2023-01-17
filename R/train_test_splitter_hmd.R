#' Splitting a Dataframe to Training and Test Datasets
#' @param data An arbitrary dataframe
#' @param proportion Proportion of train to test. Default is 0.7
#' @param join The common column of train and test, by which separation is based on.
#' @export
#' @return Returns a hash containing train and test data
#' @details
#' This functions allows the user to split data into train and test. The function
#' needs a common column in both train and test, on which the separation is based.
#' In car_insurance_data, the join column is ID
#' @examples
#' h <- train_test_splitter_hmd(car_insurance_data, proportion=0.7, join="ID")
#' train <- h$train
#' test <- h$test
#' dplyr::glimpse(train)
#' dplyr::glimpse(test)


train_test_splitter_hmd <- function(data=aiinsurance::car_insurance_data,
                              proportion=0.7,
                              join="ID"){
  # ensure dataframe is not empty
  if (!(is.data.frame({{data}}))){
    stop("data input argument should be a dataframe.")
  }

  # make a copy of data with different pointer in memory
  df <- data.frame({{data}})

  if (!(is.character({{join}}))){
    stop("join input join should be of type character.")
  }
  if (!({{join}} %in% names(df))){
    stop("join input argument should be contained in dataframe from data input argument.")
  }


  df_train <- df |> dplyr::sample_frac({{proportion}})
  df_test  <- dplyr::anti_join(df, df_train, by = {{join}})
  #d <- c("train"=df_train, "test" = df_test)
  h <- hash::hash()
  h[["train"]] <- df_train
  h[["test"]] <- df_test
  return(h)
}

