context("test-vis_numerics")

test_that("vis_numerics works", {

  ggp <- vis_numerics(rnorm(100), .plot = FALSE)
  expect_is(ggp, class = c("gg", "ggplot"))

  # Test if there is not numeric variabl
  expect_error(vis_numerics(letters, .plot = FALSE),
              ".num should be a numeric vector")
})
