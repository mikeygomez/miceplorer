library(testthat)

test_that("preprocessdata handles bodyweight data correctly", {
  test_data <- data.frame(
    "ID" = c("M_1", "M_2"),
    "Body Weight 1" = c(20, 21),
    "Date Body Weight 1" = as.Date(c("2024-01-01", "2024-01-01")),
    "Body Weight 2" = c(22, 23),
    "Date Body Weight 2" = as.Date(c("2024-01-02", "2024-01-02")),
    check.names = FALSE
  )

  result <- preprocessdata(test_data, "Body Weight")

  expect_equal(dim(result), c(2, 2))
  expect_equal(colnames(result), c("M_1", "M_2"))
  expect_equal(rownames(result), c("2024-01-01", "2024-01-02"))
  expect_equal(unname(result[1,]), c(20, 21))
  expect_equal(unname(result[2,]), c(22, 23))
})

test_that("preprocessdata handles outcome data correctly", {
  test_data <- data.frame(
    "ID" = c("M_1", "M_2"),
    "Outcome 1" = c(1, 2),
    "Date Outcome 1" = as.Date(c("2024-01-01", "2024-01-01")),
    check.names = FALSE
  )

  result <- preprocessdata(test_data, "Outcome")

  expect_equal(dim(result), c(1, 2))
  expect_equal(colnames(result), c("M_1", "M_2"))
  expect_equal(rownames(result), "2024-01-01")
  expect_equal(unname(result[1,]), c(1, 2))
})

