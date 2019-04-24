#' Box plot of a numerical column by groups of a categorical column in a dataframe
#'
#' @param .data a dataframe
#' @param .cat a catagorical vector
#' @param .num a numeric vector
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr select group_by mutate pull
#' @importFrom ggplot2 enquo ggplot aes geom_boxplot theme_classic theme element_text scale_fill_viridis_c labs quo_name
#' @importFrom stats median reorder
#'
#' @examples
#' plot_box(iris, Species, Sepal.Length)
plot_box <- function(.data, .cat, .num) {

  varx <- enquo(.cat)
  vary <- enquo(.num)

  df <- select(.data, !!varx, !!vary)

  med <- df %>%
    group_by(!!varx) %>%
    mutate(Median = median(!!vary, na.rm = TRUE)) %>%
    pull(Median)

  ggp <- ggplot(df, aes(reorder(!!varx, !!vary, median, na.rm = TRUE), !!vary, fill = med)) +
    geom_boxplot(show.legend = FALSE, color = "#555555", na.rm = TRUE) +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10))  +
    scale_fill_viridis_c() +
    labs(x = quo_name(varx))
  # Return plot
  return(ggp)
}
