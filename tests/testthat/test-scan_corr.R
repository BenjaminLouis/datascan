context("test-scan_corr")

test_that("scan_corr works", {

  data(iris)

  # Test if there are numeric variables
  res <- scan_corr(iris)
  expect_is(res, class = "data.frame")

  # Test if there is not numeric variable
  expect_error(scan_corr(iris["Species"]),
               "There should be at least 2 numerical columns in .data")

})
