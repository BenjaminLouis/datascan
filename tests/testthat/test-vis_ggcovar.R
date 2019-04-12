context("test-vis_ggcovar")

test_that("vis_ggcovar works", {

  library(dplyr)
  ggplist <- vis_ggcovar(starwars, skin_color)
  expect_is(ggplist, class = "list")
  expect_is(ggplist[[1]], class = c("gg", "ggplot"))

})
