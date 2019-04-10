context("test-vis_numerics")

test_that("vis_numerics works", {

  data(iris)

  # Test if there are numeric variables
  ggplist <- vis_numerics(iris)
  expect_is(ggplist, class = "list")

  # Test if there is not numeric variable
  expect_error(vis_numerics(iris["Species"]),
               "No numeric columns found")
})
