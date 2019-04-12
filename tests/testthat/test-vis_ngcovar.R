context("test-vis_ngcovar")

test_that("vis_ngcovar works", {

  library(dplyr)
  ggplist <- vis_ngcovar(starwars, height)
  expect_is(ggplist, class = "list")
  expect_is(ggplist[[1]], class = c("gg", "ggplot"))

})
