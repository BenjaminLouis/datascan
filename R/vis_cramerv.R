#' Plot bias corrected Cramer's V between categorical columns of a dataframe
#'
#' @param .data a data frame
#'
#' @return a ggplot
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile theme_classic theme element_blank element_text scale_fill_gradient2 scale_y_discrete scale_x_discrete labs
#'
#' @examples
#' library(dplyr)
#' vis_cramerv(starwars)
vis_cramerv <- function(.data) {
  result <- scan_cramerv(.data)

  ggplot(result, aes(Cat1, Cat2, fill = cramerV)) +
    geom_tile() +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 10),
          legend.position = "right",
          legend.title = element_text(face = "bold", size = 12))  +
    # scale_fill_viridis_c(option = "E", limits = c(0, 1),breaks = c(0, 0.5, 1),
    #                      direction = -1)  +
    scale_fill_gradient2(low = "#EAEDE9", mid = "gold2", high = "#601200",
                         breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1),
                         midpoint = 0.5, na.value = "grey") +
    scale_y_discrete(expand = c(0, 0),
                     limits = sort(unique(result$Cat2), decreasing = TRUE)) +
    scale_x_discrete(expand = c(0, 0), position = "top",
                     limits = sort(unique(result$Cat1), decreasing = FALSE)) +
    labs(fill = "Cramer's V")
}
