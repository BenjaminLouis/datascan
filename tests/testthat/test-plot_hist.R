context("test-plot_hist")

test_that("plot_hist works", {

  ggp <- plot_hist(iris, Sepal.Length)
  expect_is(ggp, class = c("gg", "ggplot"))

  # Test if there is not numeric variabl
  expect_error(plot_hist(iris, Species),
              ".num should be a numerical column")
})
