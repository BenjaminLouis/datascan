#' Visualise frequency of paired group between categorical variables in a data frame
#'
#' @param .data a data frame
#' @param .regroup if \code{TRUE}, groups xith less than 5 occurences are grouped together under the name "Others_groups"
#'
#' @return a list of ggplot
#' @export
#'
#' @importFrom dplyr select_if mutate_at select filter transmute pull
#' @importFrom ggplot2 vars syms
#' @importFrom purrr map2
#' @importFrom tidyr crossing
#'
#' @examples
#' library(dplyr)
#' vis_groupsbygroups(starwars)
vis_groupsbygroups <- function(.data, .regroup = TRUE) {

  catname <- select_if(.data, ~!is.numeric(.x) & !is.list(.x)) %>%
    select_if(~any(table(.x) > 1)) %>%
    colnames()

  if (.regroup) {
    .data <- .data %>%
      mutate_at(vars(!!!syms(catname)), .transform_cat)
  }

  if (length(catname) < 2) {
    stop("There should be at least two categorical variables")
  }

  df <- select(.data, !!!syms(catname))
  crossing(Cat1 = catname, Cat2 = catname) %>%
    filter(Cat1 != Cat2) %>%
    transmute(ggplist = map2(Cat1, Cat2, ~vis_grpbygrp(pull(df, .x),
                                   pull(df, .y),
                                   .labx = .x,
                                   .laby = .y))) %>%
    pull(ggplist)

}
