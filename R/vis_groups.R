#' Bar plots of all categorical columns in a dataframe
#'
#' @param .data a dataframe
#'
#' @return a list of ggplots
#' @export
#'
#' @importFrom dplyr select_if
#' @importFrom ggplot2 syms
#' @importFrom purrr map
#'
#' @examples
#' library(dplyr)
#' vis_groups(starwars)
vis_groups <- function(.data) {
  # Select cat columns
  catdata <- select_if(.data, ~is.character(.x) | is.factor(.x))
  # There should be numeric colmns
  if (ncol(catdata) == 0) {stop("There should be categorical columns in .data")}
  # Get the distr
  map(syms(names(catdata)), plot_bar, .data = catdata)

}
