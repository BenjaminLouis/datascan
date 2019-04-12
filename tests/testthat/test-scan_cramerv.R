context("test-scan_cramerv")

test_that("scan_cramerv works", {

  library(dplyr)
  expect_is(scan_cramerv(starwars), class = "data.frame")

  expect_error(scan_cramerv(iris),
               "There should be at least 2 categorical columns in .data")

})
