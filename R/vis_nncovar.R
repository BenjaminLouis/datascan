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
#' @importFrom purrr map2 map
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

  target <- ggplot2::quos(...)
  target_name <- sapply(target, ggplot2::quo_name)

  df <- dplyr::select_if(.data, is.numeric)
  non_target_name <- dplyr::setdiff(colnames(df), target_name)

  if (length(non_target_name) == 0) {
    target_name <- NULL
    warning("All numerical columns were targets. Argument ... was ignored")
  }

  if (any(!target_name %in% colnames(df))) {
    stop("All target columns (...) should be numerical")
  }

  if (length(target_name) > 0) {
    dfcross <- tidyr::crossing(x = non_target_name, y = target_name) %>%
      dplyr::mutate(data = purrr::map2(x, y, ~dplyr::select(df, .x, .y))) %>%
      dplyr::pull(data)
  } else {
    dfcross <- utils::combn(non_target_name, 2, simplify = FALSE) %>%
      purrr::map(dplyr::select, .data = df)
  }

  dfcross %>%
    purrr::map(~plot_scatter(.x, .num1 = !!ggplot2::sym(names(.x)[1]), .num2 = !!ggplot2::sym(names(.x)[2])))

}
