context("test-vis_cramerv")

test_that("vis_cramerv works", {

  library(dplyr)
  expect_is(vis_cramerv(starwars), class = c("gg", "ggplot"))

})
