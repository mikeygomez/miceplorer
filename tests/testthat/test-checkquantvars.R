library(testthat)

test_that("checkquantvars handles valid values correctly", {
  test_data <- data.frame(
    "ID" = c("A1", "A2", "A3", "A4"),
    "Body Weight 1" = c(20, 21, 22, 23),
    check.names = FALSE
  )

  local_mocked_bindings(
    readline = function(...) "21",
    cat = function(...) invisible(),
    .package = "base"
  )

  result <- checkquantvars(test_data, "Body Weight")
  expect_equal(as.numeric(result[,2]), c(20, 21, 22, 23))
})

test_that("checkquantvars handles invalid values correctly", {
  test_data <- data.frame(
    "ID" = c("A1", "A2", "A3", "A4"),
    "Body Weight 1" = c(20, "Dead", 22, 23),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    readline = function(...) "NA",
    cat = function(...) invisible(),
    .package = "base"
  )

  result <- checkquantvars(test_data, "Body Weight")
  expect_equal(result[,2], c(20, NA, 22, 23))
})
