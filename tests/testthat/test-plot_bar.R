context("test-plot_bar")

test_that("plot_bar works", {

  library(dplyr)
  expect_is(plot_bar(starwars, eye_color),
            class = c("gg", "ggplot"))

  expect_error(plot_bar(iris, Sepal.Length),
               ".cat should be a categorical column")

})
