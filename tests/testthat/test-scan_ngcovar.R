context("test-scan_ngcovar")

test_that("scan_ngcovar works", {

  expect_is(scan_ngcovar(iris), class = "data.frame")

  expect_error(scan_ngcovar(iris[,1:4]),
               "There should be at least one numerical and one categorical column in .data")

})
