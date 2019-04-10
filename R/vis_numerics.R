#' Get and Visualise distribution of all numeric column in a data frame
#'
#' @param .data a data frame
#'
#' @return a list of ggplot objects
#' @export
#'
#' @importFrom dplyr select_if
#'
#' @examples
#' vis_numerics(iris)
vis_numerics <- function(.data) {
  # Select numeric columns
  numdata <- select_if(.data, is.numeric)
  # There should be numeric colmns
  if (ncol(numdata) == 0) {stop("No numeric columns found")}
  # Get the distr
  ggplist <- mapply(vis_num, .num = numdata, .labx = names(numdata), SIMPLIFY = FALSE)
  # return the distr
  return(ggplist)
}
