context("test-scan_numerics.R")

test_that("scan-numerics works", {

  data(iris)

  # Test if there are numeric variables
  res <- scan_numerics(iris)
  expect_is(res, class = "data.frame")

  # Test if there is not numeric variable
  expect_error(scan_numerics(iris["Species"]),
               "No numeric variables found")
})
