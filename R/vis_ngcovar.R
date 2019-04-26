#' Violin plots for all pairs of numerical/categorical columns in a data frame
#'
#' @param .data a dataframe
#' @param ... you can use this arguement to specify targeted numerical columns that will be used as y-axis variable
#' @param .regroup if \code{TRUE}, groups with less than 5 occurences are grouped together under the name "Others_groups"
#' @param type either "violin" for violin plots or "box" for boxplots
#' @param .by categorical columns names to add a second categorical column
#'
#' @return a list of ggplot
#' @export
#'
#' @importFrom dplyr select_if select mutate_at vars mutate pull
#' @importFrom rlang quos quo_name syms sym
#' @importFrom purrr map_chr map2 map pmap
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
vis_ngcovar <- function(.data, ..., .regroup = TRUE, type = "violin", .by = NULL) {

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

  if (!is.null(.by)) {
    catname <- setdiff(catname, .by)
    if (length(catname) == 0) {
      catname <- .by
      .by = NULL
    }
  }

  if (length(numname) == 0 | length(catname) == 0) {
    stop("There should be at least one numerical and one categorical columns")
  }

  if (.regroup) {
    df <- .data %>%
      select(!!!syms(catname), !!!syms(numname), !!!syms(.by)) %>%
      mutate_at(vars(!!!syms(catname)), .transform_cat)
  } else {
    df <- .data %>%
      select(!!!syms(catname), !!!syms(numname), !!!syms(.by))
  }

  dfcross <- crossing(x = catname, y = numname, by = .by)
  if (is.null(.by)) {
    dt <-  dfcross %>%
      mutate(data = map2(x, y, ~select(df, .x, .y))) %>%
      pull(data)
      if (type == "violin") {
        map(dt, ~plot_violin(.x, .cat = !!sym(names(.x)[1]), .num = !!sym(names(.x)[2])))
      } else if (type == "box") {
        map(dt, ~plot_box(.x, .cat = !!sym(names(.x)[1]), .num = !!sym(names(.x)[2])))
      }
  } else {
    dt <- dfcross %>%
      mutate(data = pmap(list(x, y, by), ~select(df, ..1, ..2, ..3))) %>%
      pull(data)
    if (type == "violin") {
      map(dt, ~plot_violin(.x, .cat = !!sym(names(.x)[1]), .num = !!sym(names(.x)[2]),
                           .by = !!sym(names(.x)[3])))
    } else if (type == "box") {
      map(dt, ~plot_box(.x, .cat = !!sym(names(.x)[1]), .num = !!sym(names(.x)[2]),
                        .by = !!sym(names(.x)[3])))
    }

  }


}
