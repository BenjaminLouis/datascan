#' Scatter plots of all pairs of numerical variables in a data.frame
#'
#' @param .data a data frame
#' @param ... you can use this arguement to specify targeted numerical columns that will be used as y-axis variable
#'
#' @return a list of ggplots
#' @export
#'
#' @importFrom dplyr select_if setdiff mutate select pull
#' @importFrom ggplot2 quos quo_name sym
#' @importFrom purrr map2 map map_chr
#' @importFrom tidyr crossing
#' @importFrom utils combn
#'
#' @examples
#' # All combinations
#' vis_nncovar(iris)
#'
#' # With a targeted y-axis variable
#' vis_nncovar(iris, Sepal.Length)
vis_nncovar <- function(.data, ...) {

  target <- quos(...)
  target_name <- map_chr(target, quo_name)

  df <- select_if(.data, is.numeric)
  non_target_name <- setdiff(colnames(df), target_name)

  if (length(non_target_name) == 0) {
    non_target_name <- target_name
    target_name <- NULL
    warning("All numerical columns were targets. Argument 'dots' was ignored")
  }

  if (any(!target_name %in% colnames(df))) {
    stop("All target columns should be numerical")
  }

  if (length(target_name) > 0) {
    dfcross <- crossing(x = non_target_name, y = target_name) %>%
      mutate(data = map2(x, y, ~select(df, .x, .y))) %>%
      pull(data)
  } else {
    dfcross <- combn(non_target_name, 2, simplify = FALSE) %>%
      map(select, .data = df)
  }

  dfcross %>%
    map(~plot_scatter(.x, .num1 = !!sym(names(.x)[1]), .num2 = !!sym(names(.x)[2])))

}
