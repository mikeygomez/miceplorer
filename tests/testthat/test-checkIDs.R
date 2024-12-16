library(testthat)

test_that("checkIDs handles valid IDs correctly", {
  test_data <- data.frame(
    "ID" = c("A1", "A2", "A3", "A4"),
    "Body Weight 1" = c(20, 21, 22, 23),
    check.names = FALSE
  )

  birth_data <- data.frame(
    "ID" = c("A1", "A2", "A3", "A4"),
    check.names = FALSE
  )

  local_mocked_bindings(
    cat = function(...) invisible(),
    .package = "base"
  )

  result <- checkIDs(test_data, birth_data, "Body Weight")
  expect_identical(result$data$ID, c("A1", "A2", "A3", "A4"))
  expect_null(result$summary$invalid_ids)
})

test_that("checkIDs handles invalid IDs", {
  test_data <- data.frame(
    "ID" = c("A1", "A5", "A3", "A6"),
    "Body Weight 1" = c(20, 21, 22, 23),
    check.names = FALSE
  )

  birth_data <- data.frame(
    "ID" = c("A1", "A2", "A3", "A4"),
    check.names = FALSE
  )

  local_mocked_bindings(
    readline = function(...) "1",
    cat = function(...) invisible(),
    .package = "base"
  )

  result <- checkIDs(test_data, birth_data, "Body Weight")
  expect_identical(result$data$ID, c("A1", "A2", "A3", "A4"))
  expect_equal(result$summary$changes_made, 2)
})
