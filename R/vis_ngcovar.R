#' Violin plots for all pairs of numerical/categorical columns in a data frame
#'
#' @param .data a dataframe
#' @param ... you can use this arguement to specify targeted numerical columns that will be used as y-axis variable
#' @param .regroup if \code{TRUE}, groups with less than 5 occurences are grouped together under the name "Others_groups"
#' @param type either "violin" for violin plots or "box" for boxplots
#'
#' @return a list of ggplot
#' @export
#'
#' @importFrom dplyr select_if select mutate_at vars mutate pull
#' @importFrom ggplot2 quos quo_name syms sym
#' @importFrom purrr map_chr map2 map
#' @importFrom tidyr crossing
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # By regrouping small groups
#' vis_ngcovar(starwars)
#'
#' # Without regrouping small groups
#' vis_ngcovar(starwars, .regroup = FALSE)
#'
#' # With a target column
#' vis_ngcovar(starwars, height)
#' }
vis_ngcovar <- function(.data, ..., .regroup = TRUE, type = "violin") {

  target <- quos(...)
  target_name <- map_chr(target, quo_name)

  if (length(target_name) == 0) {
    numname <- select_if(.data, is.numeric) %>% colnames()
  } else {
    numname <- target_name
  }

  catname <- select_if(.data, ~is.factor(.x) | is.character(.x)) %>%
    select_if(~any(table(.x) > 1)) %>% #This line should be explained
    colnames()

  if (length(numname) == 0 | length(catname) == 0) {
    stop("There should be at least one numerical and one categorical columns")
  }

  if (.regroup) {
    df <- .data %>%
      select(!!!syms(catname), !!!syms(numname)) %>%
      mutate_at(vars(!!!syms(catname)), .transform_cat)
  } else {
    df <- .data %>%
      select(!!!syms(catname), !!!syms(numname))
  }

  dt <- crossing(x = catname, y = numname) %>%
    mutate(data = map2(x, y, ~select(df, .x, .y))) %>%
    pull(data)
  if (type == "violin") {
    map(dt, ~plot_violin(.x, .cat = !!sym(names(.x)[1]), .num = !!sym(names(.x)[2])))
  } else if (type == "box") {
    map(dt, ~plot_box(.x, .cat = !!sym(names(.x)[1]), .num = !!sym(names(.x)[2])))
  }


}
