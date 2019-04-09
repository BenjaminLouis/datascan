context("test-vis_num")

test_that("vis_num works", {

  ggp <- vis_num(rnorm(100), .plot = FALSE)
  expect_is(ggp, class = c("gg", "ggplot"))

  # Test if there is not numeric variabl
  expect_error(vis_num(letters, .plot = FALSE),
              ".num should be a numeric vector")
})
