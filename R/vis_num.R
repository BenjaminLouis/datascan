#' Get and Visualise distribution of a numeric vector
#'
#' @param .num a numeric vector
#' @param .labx label of x-axix
#' @param .bins bins argument for \code{geom_histogram}
#'
#' @return a ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot geom_histogram aes labs theme_classic theme element_text scale_y_continuous scale_fill_viridis_d
#'
#' @examples
#' vis_num(rnorm(100))
vis_num <- function(.num, .labx = "", .bins = .get_bins(.num)) {
  # Test if numeric
  if (!is.numeric(.num)) {stop(".num should be a numeric vector")}
  # Remove NAs
  .num <- .num[!is.na(.num)]
  # Get the plot
  ggp <- ggplot() +
    geom_histogram(aes(.num, fill = ".num"), show.legend = FALSE,
                   bins = .bins, boundary = min(.num), color = "#555555") +
    labs(x = .labx, y = "Count") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10))  +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_fill_viridis_d(option = "E")
  # Return plot
  return(ggp)
}







