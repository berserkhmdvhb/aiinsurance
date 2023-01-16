test_that("Output Type Checking", {
  df <- aiinsurance::car_insurance_data |> categoricals_hmd()
  expect_true(is.data.frame(df))
})

test_that("Factorise checking", {
  # making sure all character columns are now as factors
  df <- aiinsurance::car_insurance_data |> categoricals_hmd()
  char_cols <- sapply(df, class) == "character"
  expect_false(any(char_cols))
})
