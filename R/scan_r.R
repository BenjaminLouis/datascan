#' Get the R coefficient of linear models between numeric and catagorical variables
#'
#' @param .data a data frame
#'
#' @return a data frame with paired names of categorical variables (\code{Cat}
#' and mnumeric ones \code{Num} and \code{r} value)
#' @export
#'
#' @importFrom dplyr select_if mutate
#' @importFrom purrr map2_dbl
#' @importFrom tidyr crossing
#'
#' @examples
#' vis_r(dplyr::starwars)
scan_r <- function(.data) {
  quanti <- .data %>%
    select_if(is.numeric) %>%
    colnames()
  quali <- .data %>%
    select_if(function(x) !is.numeric(x) & !is.list(x)) %>%
    colnames()
  df <- .data
  result <- crossing(Num = quanti, Cat = quali) %>%
    mutate(r = map2_dbl(Cat, Num, .get_r, .data = df))
  return(result)
}

