context("test-plot_scatter")

test_that("plot_scatter works", {

  expect_is(plot_scatter(iris, Sepal.Length, Sepal.Width),
            class = c("gg", "ggplot"))

  expect_is(plot_scatter(iris, Sepal.Length, Sepal.Width, .by = Species),
  class = c("gg", "ggplot"))

})
