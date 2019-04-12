#' Statistics of numerics columns by groups of catagorical columns
#'
#' @param .data a data frame
#'
#' @return a data frame with identifiers for numerics and categorical (and groups) columns
#' and the same summary values as \link[datascan]{scan_numerics}
#' @export
#'
#' @importFrom dplyr select_if syms group_by mutate select everything arrange
#' @importFrom purrr map_int map
#' @importFrom tidyr gather nest unnest
#'
#' @examples
#' scan_numbygrp(iris)
scan_numbygrp <- function(.data) {

  numname <- select_if(.data, is.numeric) %>% colnames()
  catname <- select_if(.data, ~!is.numeric(.x) & !is.list(.x)) %>% colnames()


  if (length(numname) == 0 | length(catname) == 0) {
    stop("There should be at least one numerical and one categorical variable")
  }

  .data %>%
    gather(key = "Cat", value = "Groups", !!!syms(catname)) %>%
    group_by(Cat, Groups) %>%
    nest(!!!syms(numname)) %>%
    mutate(n = map_int(data, nrow)) %>%
    mutate(summary = map(data, scan_numerics)) %>%
    unnest(summary) %>%
    select(Num = Var, everything()) %>%
    arrange(Num)

}
