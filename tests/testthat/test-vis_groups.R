context("test-vis_groups")

test_that("vis_groups works", {

  ggp <- vis_groups(sample(letters[1:5], 100, replace = TRUE), .plot = FALSE)
  expect_is(ggp, class = c("gg", "ggplot"))

  # Test if there is not categorical variabl
  expect_warning(vis_groups(rnorm(100), .plot = FALSE),
               ".cat should not be a numeric vector")

})
