library(testthat)

test_that("create_treatment_info handles valid input correctly", {
  birth_data <- data.frame(
    "ID" = c("A1", "A2", "A3"),
    "Treatment" = c("Plac", "Tmt", "Plac"),
    "OtherCol" = c(1, 2, 3),
    check.names = FALSE
  )

  result <- create_treatment_info(birth_data)
  expect_equal(colnames(result), c("ID", "Treatment"))
  expect_equal(nrow(result), 3)
})

test_that("create_treatment_info errors on missing columns", {
  bad_data <- data.frame(
    "ID" = c("A1", "A2", "A3"),
    "OtherCol" = c(1, 2, 3),
    check.names = FALSE
  )

  expect_error(create_treatment_info(bad_data), "Birth data must contain 'ID' and 'Treatment' columns")
})
