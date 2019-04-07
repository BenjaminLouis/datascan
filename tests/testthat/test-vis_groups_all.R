context("test-vis_groups_all")

test_that("vis_groups_all works", {

  data(iris)

  # Test if there are numeric variables
  ggplist <- vis_groups_all(iris, .plot = FALSE)
  expect_is(ggplist, class = "list")

  # Test if there is not numeric variable
  expect_error(vis_groups_all(iris[1:4], .plot = FALSE),
               "No categorical columns found")

})
