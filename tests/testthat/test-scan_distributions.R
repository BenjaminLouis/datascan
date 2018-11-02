context("test-scan_distributions.R")

test_that("scan-distributions works", {

  data(iris)

  # Test if there are numeric variables
  res <- scan_distributions(iris)
  expect_is(res, class = "data.frame")

  # Test if there is not numeric variable
  expect_error(scan_distributions(iris["Species"]),
               "No numeric variables found")
})
