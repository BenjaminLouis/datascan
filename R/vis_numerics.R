#' Distribution of all numerical columns in a dataframe
#'
#' @param .data a data frame
#'
#' @return a list of ggplot objects
#' @export
#'
#' @importFrom dplyr select_if
#' @importFrom rlang syms
#' @importFrom purrr map
#'
#' @examples
#' vis_numerics(iris)
vis_numerics <- function(.data) {
  # Select numeric columns
  numdata <- select_if(.data, is.numeric)
  # There should be numeric colmns
  if (ncol(numdata) == 0) {stop("There should be numerical columns in .data")}
  # Get the distr
  map(syms(names(numdata)), plot_hist, .data = numdata)
}

