#' Get bias corrected Cramer's V for all pairs of categorical columns
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

  quali <- .data %>%
    select_if(function(x) is.character(x) | is.factor(x)) %>%
    colnames()
  df <- .data
  result <- crossing(Cat1 = quali, Cat2 = quali) %>%
    mutate(cramerV = map2_dbl(Cat1, Cat2, ~.get_cramerv(df[[.x]], df[[.y]])))

  return(result)
}
