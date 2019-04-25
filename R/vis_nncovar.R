#' Scatter plots of all pairs of numerical variables in a data.frame
#'
#' @param .data a data frame
#' @param ... you can use this arguement to specify targeted numerical columns that will be used as y-axis variable
#' @param .by categorical columns names to color points by groups
#'
#' @return a list of ggplots
#' @export
#'
#' @importFrom dplyr select_if setdiff mutate select pull tibble
#' @importFrom ggplot2 quos quo_name sym
#' @importFrom purrr map2 map map_chr pmap
#' @importFrom tidyr crossing
#' @importFrom utils combn
#'
#' @examples
#' # All combinations
#' vis_nncovar(iris)
#'
#' # With a targeted y-axis variable
#' vis_nncovar(iris, Sepal.Length)
vis_nncovar <- function(.data, ..., .by = NULL) {

  target <- quos(...)
  target_name <- map_chr(target, quo_name)

  dt <- .data
  df <- select_if(.data, is.numeric)
  non_target_name <- setdiff(colnames(df), target_name)

  if (.by == TRUE) {
    .by <- select_if(.data, ~is.character(.x) | is.factor(.x)) %>% colnames()
  }

  if (length(non_target_name) == 0) {
    non_target_name <- target_name
    target_name <- NULL
    warning("All numerical columns were targets. Argument 'dots' was ignored")
  }

  if (any(!target_name %in% colnames(df))) {
    stop("All target columns should be numerical")
  }

  if (length(target_name) > 0) {
    dfcross <- crossing(x = non_target_name, y = target_name, by = .by)
  } else {
    comb <- combn(non_target_name, 2, simplify = FALSE)
    dfcross <- crossing(dt = 1:length(comb), by = .by)  %>%
      mutate(dt = map(dt, ~tibble(x = comb[[.x]][1], y = comb[[.x]][2]))) %>%
      unnest(dt) %>%
      select(x, y, everything())
  }

  if (is.null(.by)) {
    dfcross %>%
      mutate(data = map2(x, y, ~select(dt, .x, .y))) %>%
      pull(data) %>%
      map(~plot_scatter(.x, .num1 = !!sym(names(.x)[1]), .num2 = !!sym(names(.x)[2])))
  } else {
    dfcross %>%
      mutate(data = pmap(list(x, y, by), ~select(dt, ..1, ..2, ..3))) %>%
      pull(data) %>%
      map(~plot_scatter(.x, .num1 = !!sym(names(.x)[1]), .num2 = !!sym(names(.x)[2]), .by = !!sym(names(.x)[3])))
  }


}
