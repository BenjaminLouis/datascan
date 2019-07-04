#' General summary values about a dataframe
#'
#' @param .data a dataframe
#'
#' @return a datframe with the summary values
#' @export
#'
#' @importFrom dplyr as_tibble select
#' @importFrom purrr map_chr
#'
#' @examples
#' scan_data(iris)
scan_data <- function(.data) {
  rbind(
    c("Rows", nrow(.data)),
    c("Columns", ncol(.data)),
    map_chr(.data, ~class(.x)[1]) %>%
      table() %>%
      as_tibble(),
    c("All NAs rows",
      apply(.data, 1, function(a) all(is.na(a))) %>%
        sum()),
    c("All NAs columns",
      apply(.data, 2, function(a) all(is.na(a))) %>%
        sum())
  ) %>%
    select(Object = ".", N = n)
}
