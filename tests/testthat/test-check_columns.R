library(testthat)

test_that("check_columns handles correct column names", {
  test_data <- data.frame(
    "ID" = 1:3,
    "Body Weight 1" = c(20, 21, 22),
    "Date Body Weight 1" = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    check.names = FALSE
  )

  local_mocked_bindings(
    readline = function(...) "n",
    cat = function(...) invisible(),
    .package = "base"
  )

  result <- check_columns(test_data, "Body Weight")
  expect_identical(colnames(result$data), c("ID", "Body Weight 1", "Date Body Weight 1"))
})

test_that("check_columns fixes incorrect column names", {
  test_data <- data.frame(
    "ID" = 1:3,
    "incorrect_weight" = c(20, 21, 22),
    "Date Body Weight 1" = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    check.names = FALSE
  )

  local_mocked_bindings(
    readline = function(...) "y",
    cat = function(...) invisible(),
    .package = "base"
  )

  result <- check_columns(test_data, "Body Weight")
  expect_identical(colnames(result$data), c("ID", "Body Weight 1", "Date Body Weight 1"))
})
