#' Scatter plot between two numerical columns of a data frame
#'
#' @param .data a data frame
#' @param .num1 unquoted name of numercial column for x-axis
#' @param .num2 unquoted name of numercial column for y-axis
#' @param .by default \code{NULL}. Unqoted name of categorical column to color points by groups
#'
#' @return a ggplot
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme_classic theme element_text scale_color_viridis_d
#' @importFrom rlang enquo quo_name
#'
#' @examples
#' # With .by = NULL
#' plot_scatter(iris, Sepal.Length, Sepal.Width)
#'
#' # With color by groups
#' plot_scatter(iris, Sepal.Length, Sepal.Width, .by = Species)
plot_scatter <- function(.data, .num1, .num2, .by = NULL) {

  x = enquo(.num1)
  y = enquo(.num2)
  by = enquo(.by)

  if (quo_name(by) != "NULL") {
    ggp <- ggplot(.data, aes(x = !!x, y = !!y, color = !!by)) +
      geom_point(shape = 19, size = 2)
  } else {
    ggp <- ggplot(.data, aes(x = !!x, y = !!y)) +
      geom_point(shape = 19, color = "#555555", size = 2)
  }

  ggp +
    labs(x = quo_name(x), y = quo_name(y)) +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10),
          legend.position = "right") +
    scale_color_viridis_d()

}
