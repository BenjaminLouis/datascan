context("test-vis_nncovar")

test_that("vis_nncovar works", {

  ggplist <- vis_nncovar(iris)
  expect_is(ggplist, class = "list")
  expect_is(ggplist[[1]], class = c("gg", "ggplot"))

  expect_error(vis_nncovar(iris, Species),
               "All target columns should be numerical")

  expect_warning(vis_nncovar(iris, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
                 "All numerical columns were targets. Argument 'dots' was ignored")

})
