context("test-vis_r")

test_that("vis_r works", {

  library(dplyr)
  expect_is(vis_r(starwars), class = c("gg", "ggplot"))

})
