library(testthat)

test_that("identifyoutliers validates inputs and returns correct structure", {
  test_data <- data.frame(
    "ID" = c("M_1", "M_2", "M_3"),
    "Body Weight 1" = c(20, 20, 21),
    "Date Body Weight 1" = as.Date(rep("2024-01-01", 3)),
    "Body Weight 2" = c(21, 22, 23),
    "Date Body Weight 2" = as.Date(rep("2024-01-02", 3)),
    check.names = FALSE
  )

  treatment_info <- data.frame(
    "ID" = c("M_1", "M_2", "M_3"),
    "Treatment" = c("Plac", "Tmt", "Plac"),
    check.names = FALSE
  )

  local_mocked_bindings(
    print = function(...) invisible(),
    message = function(...) invisible(),
    .package = "base"
  )

  result <- identifyoutliers(test_data, "Body Weight", treatment_info)

  expect_s3_class(result$model, "gls")
  expect_s3_class(result$plot, "ggplot")
  expect_named(result, c("model", "outliers", "plot"))
  expect_true(is.data.frame(result$outliers))
})

