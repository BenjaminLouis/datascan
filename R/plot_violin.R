#' Violin plot of a numerical column by groups of a categorical column in a dataframe
#'
#' @param .data a dataframe
#' @param .cat a catagorical column name
#' @param .num a numeric numerical name
#' @param .by categorical columns names to add a second categorical column
#' @param nas logical. Should missing values be considered as group on x-axis
#'
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr select group_by mutate pull
#' @importFrom ggplot2 ggplot aes geom_violin theme_classic theme element_text scale_fill_viridis_c labs
#' @importFrom rlang enquo quo_name quo
#' @importFrom stats median reorder
#'
#' @examples
#' plot_violin(iris, Species, Sepal.Length)
plot_violin <- function(.data, .cat, .num, .by, nas = TRUE) {

  varx <- enquo(.cat)
  vary <- enquo(.num)

  if (missing(.by)) {
    by <- quo(NULL)
  } else {
    by <- enquo(.by)
  }

  df <- select(.data, !!varx, !!vary, !!by)

  # med <- df %>%
  #   group_by(!!varx) %>%
  #   mutate(Median = median(!!vary, na.rm = TRUE)) %>%
  #   pull(Median)


  if (missing(.by)) {
    ggp <- ggplot(df, aes(reorder(!!varx, !!vary, median, na.rm = TRUE), !!vary,
                          fill = reorder(!!varx, !!vary, median, na.rm = TRUE))) +
      geom_violin(show.legend = FALSE, draw_quantiles = 0.5, color = "#555555", na.rm = TRUE) +
      theme_classic() +
      theme(axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(face = "bold", size = 10))  +
      scale_fill_viridis_d() +
      scale_x_discrete(na.translate = nas) +
      labs(x = quo_name(varx))
  } else {
    ggp <- ggplot(df, aes(reorder(!!by, !!vary, median, na.rm = TRUE), !!vary)) +
      geom_violin(aes(fill = !!varx), draw_quantiles = 0.5, color = "#555555", na.rm = TRUE) +
      theme_classic() +
      theme(axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(face = "bold", size = 10),
            legend.title = element_text(face = "bold", size = 12),
            legend.text = element_text(face = "bold", size = 10))  +
      scale_fill_viridis_d() +
      scale_x_discrete(na.translate = nas) +
      labs(x = quo_name(by))
  }

  # Return plot
  return(ggp)
}
