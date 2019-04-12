context("test-vis_ggcovar")

test_that("vis_ggcovar works", {

  library(dplyr)
  ggplist <- vis_ggcovar(starwars, skin_color)
  expect_is(ggplist, class = "list")
  expect_is(ggplist[[1]], class = c("gg", "ggplot"))

  expect_error(vis_ggcovar(iris),
               "There should be at least two categorical variables")

  expect_error(vis_ggcovar(starwars, height),
               "All target columns should be categorical")

  expect_warning(vis_ggcovar(starwars, hair_color, skin_color, eye_color, gender, homeworld, species),
                 "All categorical columns were targets. Argument 'dots' was ignored")

})
