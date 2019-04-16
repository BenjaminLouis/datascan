context("test-utils")

test_that("utils works", {

  #.get_madm
  set.seed(7)
  expect_equal(round(.get_madm(rnorm(100)), 2), 0.63)
  expect_equal(.get_madm(rep(1, 100)), 0)

  # .get_skewness
  set.seed(14)
  expect_equal(round(.get_skewness(rnorm(100)), 2), 0.14)

  #.get_bins
  expect_equal(.get_bins(rnorm(50)), 10)
  expect_equal(.get_bins(rnorm(100)), 15)
  expect_equal(.get_bins(rnorm(150)), 22)

  #.get_cramerv
  x <- sample(letters, 100, replace = TRUE)
  expect_equal(.get_cramerv(x, x), 1)

  #.get_r
  expect_equal(round(.get_r(iris, "Species", "Sepal.Length"), 2), 0.79)

  #.transform_cat
  library(dplyr)
  expect_true(any(.transform_cat(starwars$species) == "Others\ngroups"))


})
