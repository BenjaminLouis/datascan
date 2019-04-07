#' Get and Visualise distribution of all numeric column in a data frame
#'
#' @param .data a data frame
#' @param .plot if \code{TRUE} the plot is displayed
#'
#' @return a list of ggplot objects
#' @export
#'
#' @importFrom dplyr select_if
#' @importFrom graphics plot
#'
#' @examples
#' vis_groups_all(iris)
vis_groups_all <- function(.data, .plot = TRUE) {
  # Select numeric columns
  catdata <- select_if(.data, ~!is.numeric(.x))
  # There should be numeric colmns
  if (ncol(catdata) == 0) {stop("No categorical columns found")}
  # Get the distr
  ggplist <- mapply(vis_groups, .cat = catdata, .labx = names(catdata), SIMPLIFY = FALSE)
  # plot
  if (.plot) { lapply(ggplist, plot) }
  # return the distr
  return(ggplist)
}
