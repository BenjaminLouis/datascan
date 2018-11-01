context("test-scan_distributions.R")

test_that("scan-distributions works", {

  data(iris)
  res <- scan_distributions(iris)

  expect_is(res, class = "data.frame")
})
