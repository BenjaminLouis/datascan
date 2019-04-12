context("test-plot_count")

test_that("plot_count works", {

  library(dplyr)
  expect_is(plot_count(starwars, eye_color, hair_color),
            class = c("gg", "ggplot"))

})
