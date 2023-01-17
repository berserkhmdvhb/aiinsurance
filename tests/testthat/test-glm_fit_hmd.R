test_that("Output type checking", {
  fit <- glm_fit_hmd(aiinsurance::insurance_train)
  expect_true(setequal(fit |> class(), c("glm", "lm")))
})
