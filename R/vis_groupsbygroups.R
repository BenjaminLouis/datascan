vis_groupsbygroups <- function(.data, .regroup = TRUE) {

  catname <- select_if(.data, ~!is.numeric(.x) & !is.list(.x)) %>%
    select_if(~any(table(.x) > 1)) %>%
    colnames()

  if (.regroup) {
    .data <- .data %>%
      mutate_at(vars(!!!syms(catname)), .transform_cat)
  }

  if (length(catname) <2) {
    stop("There should be at least two categorical variables")
  }

  df <- .data
  crossing(Cat1 = catname, Cat2 = catname) %>%
    filter(Cat1 != Cat2) %>%
    transmute(ggplist = map2(Cat1, Cat2, ~vis_grpbygrp(pull(df, .x),
                                   pull(df, .y),
                                   .x, .y))) %>%
    pull(ggplist)

}
