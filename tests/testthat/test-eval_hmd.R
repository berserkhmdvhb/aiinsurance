test_that("check value ranges", {
  actual <- aiinsurance::insurance_test$outcome
  fit <- aiinsurance::glm_fit_hmd(data = aiinsurance::insurance_train,
                                  target = "outcome",
                                  family = "binomial")
  h <- aiinsurance::glm_predict_hmd(fit,
                                    data = aiinsurance::insurance_test,
                                           target = "outcome",
                                           threshold = 0.5,
                                           s = 0.5)
  predicted <- h$predictions
  h <- aiinsurance::eval_hmd(actual, predicted)
  expect_true(h$accuracy |> dplyr::between(0,1))
  expect_true(h$precision |> dplyr::between(0,1))
  expect_true(h$recall |> dplyr::between(0,1))
  expect_true(h$fbeta_score |> dplyr::between(0,1))
  expect_true(h$f1_score |> dplyr::between(0,1))
})

test_that("check output class", {
  actual <- aiinsurance::insurance_test$outcome
  fit <- aiinsurance::glm_fit_hmd(data = aiinsurance::insurance_train,
                                  target = "outcome",
                                  family = "binomial")
  h <- aiinsurance::glm_predict_hmd(fit,
                                    data = aiinsurance::insurance_test,
                                    target = "outcome",
                                    threshold = 0.5,
                                    s = 0.5)
  predicted <- h$predictions
  h <- aiinsurance::eval_hmd(actual, predicted)
  expect_true(class(h) == "hash")
})


