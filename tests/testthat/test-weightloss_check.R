library(testthat)

test_that("weightloss_check identifies mice exceeding threshold", {
  test_data <- data.frame(
    "ID" = c("M_1", "M_2"),
    "Body Weight 1" = c(20, 25),
    "Date Body Weight 1" = as.Date(c("2024-01-01", "2024-01-01")),
    "Body Weight 2" = c(14, 24),
    "Date Body Weight 2" = as.Date(c("2024-01-02", "2024-01-02")),
    check.names = FALSE
  )

  local_mocked_bindings(
    cat = function(...) invisible(),
    print = function(...) invisible(),
    .package = "base"
  )

  result <- weightloss_check(test_data, threshold = 20)

  expect_equal(nrow(result$sac_mice), 1)
  expect_equal(result$sac_mice$ID, "M_1")
  expect_equal(result$sac_mice$Percent_Change, -30)
  expect_equal(nrow(result$all_mice), 2)
})
