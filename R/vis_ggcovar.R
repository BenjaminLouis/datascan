#' Visualise frequency of paired group between categorical variables in a data frame
#'
#' @param .data a data frame
#' @param ... you can use this arguement to specify targeted categorical columns that will be used as y-axis variable
#' @param .regroup if \code{TRUE}, groups with less than 5 occurences are grouped together under the name "Others_groups"
#'
#' @return a list of ggplot
#' @export
#'
#' @importFrom dplyr select_if setdiff mutate_at vars mutate select pull
#' @importFrom rlang quos quo_name syms sym
#' @importFrom purrr map2 map map_chr
#' @importFrom tidyr crossing
#' @importFrom utils combn
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # By regrouping small groups
#' vis_ggcovar(starwars)
#'
#' # Without regrouping small groups
#' vis_ggcovar(starwars, .regroup = FALSE)
#'
#' # With a target column
#' vis_ggcovar(starwars, skin_color)
#' }
vis_ggcovar <- function(.data, ..., .regroup = TRUE) {

  target <- quos(...)
  target_name <- map_chr(target, quo_name)

  df <- select_if(.data, ~is.character(.x) | is.factor(.x)) %>%
    select_if(~any(table(.x) > 1)) # There should be a warning to explain this line
  catname <- colnames(df)

  if (length(catname) < 2) {
    stop("There should be at least two categorical variables")
  }

  non_target_name <- setdiff(catname, target_name)

  if (length(non_target_name) == 0) {
    non_target_name <- target_name
    target_name <- NULL
    warning("All categorical columns were targets. Argument 'dots' was ignored")
  }

  if (any(!target_name %in% colnames(df))) {
    stop("All target columns should be categorical")
  }

  if (.regroup) {
    df <- df %>%
      mutate_at(vars(!!!syms(catname)), .transform_cat)
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
    map(~plot_count(.x, .cat1 = !!sym(names(.x)[1]), .cat2 = !!sym(names(.x)[2])))

}
