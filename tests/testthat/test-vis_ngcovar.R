context("test-vis_ngcovar")

test_that("vis_ngcovar works", {

  library(dplyr)
  ggplist <- vis_ngcovar(starwars, height)
  expect_is(ggplist, class = "list")
  expect_is(ggplist[[1]], class = c("gg", "ggplot"))

  ggplist <- vis_ngcovar(starwars, height, .regroup = FALSE)
  expect_is(ggplist, class = "list")
  expect_is(ggplist[[1]], class = c("gg", "ggplot"))

  ggplist <- vis_ngcovar(iris)
  expect_is(ggplist, class = "list")
  expect_is(ggplist[[1]], class = c("gg", "ggplot"))

  expect_error(vis_ngcovar(iris[, 1:4]),
               "There should be at least one numerical and one categorical columns")


})
