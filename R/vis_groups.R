#' Bar plots of all categorical columns in a dataframe
#'
#' @param .data a dataframe
#' @param .by (optional) unquoted name of a categorical column
#' @param nas logical. Should missing values be considered as group on x-axis
#'
#' @return a list of ggplots
#' @export
#'
#' @importFrom dplyr select_if
#' @importFrom rlang syms quo enquo quo_name
#' @importFrom purrr map
#'
#' @examples
#' library(dplyr)
#' vis_groups(starwars)
vis_groups <- function(.data, .by, nas = TRUE) {
  # Select cat columns
  catdata <- select_if(.data, ~is.character(.x) | is.factor(.x))
  # There should be numeric colmns
  if (ncol(catdata) == 0) {stop("There should be categorical columns in .data")}
  # Select group columns
  if (missing(.by)) {
    # Get the distr
    map(syms(names(catdata)), plot_bar, .data = catdata)
  } else {
    by <- enquo(.by)
    # Get the distr
    catnames <- setdiff(names(catdata), quo_name(by))
    map(syms(catnames), plot_bar, .data = catdata, .by = !!by, nas = nas)
  }

}
