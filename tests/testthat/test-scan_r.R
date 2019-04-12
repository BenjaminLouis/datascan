context("test-scan_r")

test_that("scan_r works", {

  library(dplyr)
  expect_is(scan_r(starwars), class = "data.frame")

})
