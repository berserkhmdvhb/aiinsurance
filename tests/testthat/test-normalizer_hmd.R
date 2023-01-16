test_that("Output Type Checking", {
  h <- normalizer_hmd()
  expect_true(h |> class() == "hash")
  train <- h$train_norm
  test <- h$test_norm
  expect_true(is.data.frame(train))
  expect_true(is.data.frame(test))
})
