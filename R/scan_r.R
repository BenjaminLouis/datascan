#' R coefficient of linear models between numerical and catagorical columns in a dataframe
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
#' library(dplyr)
#' scan_r(starwars)
scan_r <- function(.data) {
  quanti <- .data %>%
    select_if(is.numeric) %>%
    colnames()
  quali <- .data %>%
    select_if(function(x) !is.numeric(x) & !is.list(x)) %>%
    colnames()
  df <- .data
  crossing(Num = quanti, Cat = quali) %>%
    mutate(r = map2_dbl(Cat, Num, .get_r, .data = df))
}

