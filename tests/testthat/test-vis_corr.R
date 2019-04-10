context("test-vis_corr")

test_that("vis_corr works", {

  data(iris)

  # Test if there are numeric variables
  ggp <- vis_corr(iris)
  expect_is(ggp, class = c("gg", "ggplot"))


})
