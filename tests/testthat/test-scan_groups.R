context("test-scan_groups")

test_that("scan-groups works", {

  data(iris)

  # Test if there are numeric variables
  res <- scan_groups(iris)
  expect_is(res, class = "data.frame")

  # Test if there is not numeric variable
  expect_error(scan_groups(iris[1:4]),
               "No categorical variables found")
})

