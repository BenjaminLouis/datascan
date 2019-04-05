context("test-vis_numerics_all")

test_that("vis_numerics_all works", {

  data(iris)

  # Test if there are numeric variables
  ggplist <- vis_numerics_all(iris, .plot = FALSE)
  expect_is(ggplist, class = "list")

  # Test if there is not numeric variable
  expect_error(vis_numerics_all(iris["Species"], .plot = FALSE),
               "No numeric columns found")
})
