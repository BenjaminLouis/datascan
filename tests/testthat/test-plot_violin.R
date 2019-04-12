context("test-plot_violin")

test_that("plot_violin works", {

  expect_is(plot_violin(iris, Species, Sepal.Length),
            class = c("gg", "ggplot"))

})
