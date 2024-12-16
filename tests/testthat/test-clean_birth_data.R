library(testthat)

test_that("clean_birth_data handles valid data correctly", {
  test_data <- data.frame(
    "ID" = c("M_1", "F_2", "M_3"),
    "Sex" = c("M", "F", "M"),
    "Treatment" = c("Plac", "Tmt", "Plac"),
    "Num" = c(1, 2, 3),
    check.names = FALSE
  )

  result <- clean_birth_data(test_data)
  expect_identical(result, test_data)
})

test_that("clean_birth_data fixes invalid treatment values", {
  test_data <- data.frame(
    "ID" = c("M_1", "F_2", "M_3"),
    "Sex" = c("M", "F", "M"),
    "Treatment" = c("Plac", "Treatment", "Plac"),
    "Num" = c(1, 2, 3),
    check.names = FALSE
  )

  local_mocked_bindings(
    readline = function(...) "2",
    cat = function(...) invisible(),
    .package = "base"
  )

  result <- clean_birth_data(test_data)
  expect_equal(result$Treatment, c("Plac", "Tmt", "Plac"))
})

test_that("clean_birth_data fixes invalid IDs", {
  test_data <- data.frame(
    "ID" = c("F_1", "M_2", "M_3"),
    "Sex" = c("M", "M", "M"),
    "Treatment" = c("Plac", "Tmt", "Plac"),
    "Num" = c(1, 2, 3),
    check.names = FALSE
  )

  local_mocked_bindings(
    readline = function(...) "1",
    cat = function(...) invisible(),
    .package = "base"
  )

  result <- clean_birth_data(test_data)
  expect_equal(result$ID, c("M_1", "M_2", "M_3"))
})
