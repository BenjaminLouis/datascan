#' Violin plot of a numerical column by groups of a categorical column in a dataframe
#'
#' @param .data a dataframe
#' @param .cat a catagorical vector
#' @param .num a numeric vector
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr select group_by mutate pull
#' @importFrom ggplot2 enquo ggplot aes geom_violin theme_classic theme element_text scale_fill_viridis_c labs quo_name
#' @importFrom stats median reorder
#'
#' @examples
#' plot_violin(iris, Species, Sepal.Length)
plot_violin <- function(.data, .cat, .num) {

  varx <- ggplot2::enquo(.cat)
  vary <- ggplot2::enquo(.num)

  df <- dplyr::select(.data, !!varx, !!vary)# %>%
   # mutate_if(~!is.numeric(.x), funs(map_chr(., ~if (is.na(.x)) {"NA"} else {.x})))

  med <- df %>%
    dplyr::group_by(!!varx) %>%
    dplyr::mutate(Median = stats::median(!!vary, na.rm = TRUE)) %>%
    dplyr::pull(Median)

  ggp <- ggplot2::ggplot(df, ggplot2::aes(stats::reorder(!!varx, !!vary, stats::median, na.rm = TRUE), !!vary, fill = med)) +
    ggplot2::geom_violin(show.legend = FALSE, draw_quantiles = 0.5, color = "#555555", na.rm = TRUE) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_text(face = "bold", size = 12),
          axis.text = ggplot2::element_text(face = "bold", size = 10))  +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x = ggplot2::quo_name(varx))
  # Return plot
  return(ggp)
}
