#' Ggplot of frequency of parired group between two categorical variables
#'
#' @param .cat1 a categorical vector
#' @param .cat2 a categorical vector
#' @param .labx label for x-axis
#' @param .laby label for y-axis
#'
#' @return a ggplot
#' @export
#'
#' @importFrom ggplot2 ggplot geom_count aes stat scale_size_area theme_classic theme element_blank element_text unit element_rect scale_color_viridis_c scale_y_discrete scale_x_discrete labs
#'
#' @examples
#' library(dplyr)
#' with(starwars, vis_grpbygrp(eye_color, hair_color))
vis_grpbygrp <- function(.cat1, .cat2, .labx = "", .laby = "") {

  ggplot() +
    geom_count(aes(.cat1, .cat2, color = stat(prop), group = 1))  +
    scale_size_area(max_size = 10, guide = FALSE) +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.key.size = unit(0.8, "cm"),
          panel.border = element_rect(color = "black", fill = "transparent"))  +
    scale_color_viridis_c(option = "D") +
    scale_y_discrete(limits = sort(unique(.cat2), decreasing = TRUE)) +
    scale_x_discrete(position = "top",
                     limits = sort(unique(.cat1), decreasing = FALSE)) +
    labs(color = "Proportion\n", x = .labx, y = .laby)

}
