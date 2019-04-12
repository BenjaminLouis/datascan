#' Bias corrected Cramer's V for all pairs of categorical columns
#'
#' @param .data a data frame
#'
#' @return a data frame with paired names of categorical variables (\code{Cat1}
#' and \code{Cat2} and Cramer's V value)
#' @export
#'
#' @importFrom dplyr select_if mutate
#' @importFrom purrr map2_dbl
#' @importFrom tidyr crossing
#'
#' @examples
#' scan_cramerv(dplyr::starwars)
scan_cramerv <- function(.data) {

  catdata <- select_if(.data, ~is.character(.x) | is.factor(.x))
  catname <- colnames(catdata)
  # There should be numeric variables
  if (ncol(catdata) == 0) {stop("No categorical variables found")}

  crossing(Cat1 = catname, Cat2 = catname) %>%
    mutate(cramerV = map2_dbl(Cat1, Cat2, ~.get_cramerv(catdata[[.x]], catdata[[.y]])))

}
