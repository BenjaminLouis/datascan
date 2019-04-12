#' Violin plots for all pairs of numeric/categorical variables in a data frame
#'
#' @name vis_numericsbygroups
#'
#' @param .data a dataframe
#' @param .regroup if \code{TRUE}, groups with less than 5 occurences are grouped together under the name "Others_groups"
#'
#' @return a list of ggplot
#' @export
#'
#' @importFrom dplyr select_if mutate_at vars syms group_by
#' @importFrom purrr pmap
#' @importFrom tidyr gather nest
#'
#' @examples
#' library(dplyr)
#' vis_numericsbygroups(starwars)
vis_numericsbygroups <- function(.data, .regroup = TRUE) {

  numname <- select_if(.data, is.numeric) %>% colnames()
  catname <- select_if(.data, ~!is.numeric(.x) & !is.list(.x)) %>%
    select_if(~any(table(.x) > 1)) %>%
    colnames()

  if (.regroup) {
    .data <- .data %>%
      mutate_at(vars(!!!syms(catname)), .transform_cat)
  }

  if (length(numname) == 0 | length(catname) == 0) {
    stop("There should be at least one numerical and one categorical variable")
  }

  ggplist <- .data %>%
    gather(key = "Cat", value = "Groups", !!!syms(catname)) %>%
    gather(key = "Num", value = "value", !!!syms(numname)) %>%
    group_by(Num, Cat) %>%
    nest(Groups, value) %>%
    pmap(~vis_numbygrp(..3$Groups, ..3$value, ..2, ..1))

  return(ggplist)
}


#' @rdname vis_numericsbygroups
#' @export
vis_nbg <- function(.data) {
  vis_numericsbygroups(.data)
}
