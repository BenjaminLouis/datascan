#' Get and Visualise distribution of a numeric vector
#'
#' @param .cat a categorical vector
#' @param .labx label of x-axix
#' @param .plot if \code{TRUE} the plot is displayed
#'
#' @return a ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot geom_bar aes labs theme_classic theme element_text scale_y_continuous scale_fill_viridis_d
#' @importFrom graphics plot
#'
#' @examples
#' vis_groups(iris[,"Species"])
vis_groups <- function(.cat, .labx = "", .plot = TRUE) {
  # Test if numeric
  if (is.numeric(.cat)) {warning(".cat should not be a numeric vector")}
  # Remove NAs
  .cat <- .cat[!is.na(.cat)]
  # Get the plot
  ggp <- ggplot() +
    geom_bar(aes(.cat, fill = .cat), color = "#555555") +
    labs(x = .labx, y = "Count") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10))  +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_fill_viridis_d()
  # Plot
  if (.plot) { plot(ggp) }
  # Return plot
  return(ggp)
}







