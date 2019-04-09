#' Plot bias corrected Cramer's V for all pairs of categorical columns
#'
#' @param .data a data frame
#'
#' @return a ggplot
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile theme_classic theme element_blank element_text scale_fill_viridis_c scale_y_discrete scale_x_discrete labs
#'
#' @examples
#' vis_cramerv(starwars)

vis_cramerv <- function(.data) {
  result <- scan_cramerv(.data)

  ggplot(result, aes(Cat1, Cat2, fill = cut(cramerV, seq(0, 1, 0.2),
                                            labels = c("0 - 0.2",
                                                       "0.2 - 0.4",
                                                       "0.4 - 0.6",
                                                       "0.6 - 0.8",
                                                       "0.8 - 1"),
                                            ordered_result = TRUE,
                                            include.lowest = TRUE,
                                            right = FALSE))) +
    geom_tile() +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 10),
          legend.position = "bottom")  +
    scale_fill_viridis_d(option = "D") +
    scale_y_discrete(expand = c(0, 0),
                     limits = sort(unique(result$Cat1), decreasing = TRUE)) +
    scale_x_discrete(expand = c(0, 0), position = "top",
                     limits = sort(unique(result$Cat1), decreasing = FALSE)) +
    labs(fill = "Cramer's V")
}
