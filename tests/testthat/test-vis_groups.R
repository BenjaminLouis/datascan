context("test-vis_groups")

test_that("vis_groups works", {

  library(dplyr)
  # Test if there are numeric variables
  ggplist <- vis_groups(starwars)
  expect_is(ggplist, class = "list")
  expect_is(ggplist[[1]], class = c("gg", "ggplot"))

  # Test if there is not numeric variable
  expect_error(vis_groups(iris[, 1:4]),
               "There should be categorical columns in .data")

})
