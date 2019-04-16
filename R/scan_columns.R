#' Summary values about columns in a dataframe
#'
#' @param .data a dataframe
#'
#' @return a dataframe with the summary values
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr map_int map_dbl
#'
#' @examples
#' library(dplyr)
#' scan_columns(starwars)
scan_columns <- function(.data) {
  df <- .data
  data.frame(Name = names(df)) %>%
    mutate(Class = map_chr(df, class)) %>%
    mutate(Unique = map_int(df, ~length(unique(.x)))) %>%
    mutate(n_na = map_int(df, ~sum(is.na(.x)))) %>%
    mutate(p_na = n_na/nrow(df)) %>%
    mutate(n_0 = map_dbl(df, ~if (is.numeric(.x)) {sum(.x == 0, na.rm = TRUE)} else {NA})) %>%
    mutate(p_0 = n_0/nrow(df)) %>%
    mutate(n_Inf = map_dbl(df, ~if (is.numeric(.x)) {sum(.x == Inf, na.rm = TRUE)} else {NA})) %>%
    mutate(p_Inf = n_Inf/nrow(df))
}
