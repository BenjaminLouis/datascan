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
#' vis_numerics_all(iris)
vis_numerics_all <- function(.data, .plot = TRUE) {
  # Select numeric columns
  numdata <- select_if(.data, is.numeric)
  # There should be numeric colmns
  if (ncol(numdata) == 0) {stop("No numeric columns found")}
  # Get the distr
  ggplist <- mapply(vis_numerics, .num = numdata, .labx = names(numdata), SIMPLIFY = FALSE)
  # plot
  if (.plot) { lapply(ggp, plot) }
  # return the distr
  return(ggplist)
}
