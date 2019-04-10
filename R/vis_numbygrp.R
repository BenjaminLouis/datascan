#' Violin plot of a numeric vector by groups of a categorical vectors
#'
#' @param .cat a catagorical vector
#' @param .num a numeric vector
#' @param .labx label for x-axis
#' @param .laby label for y-axis
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr group_by mutate pull
#' @importFrom ggplot2 ggplot geom_violin aes labs theme element_text scale_y_continuous scale_fill_viridis_c
#' @importFrom stats median reorder
#'
#' @examples
#' vis_numbygrp(iris$Species, iris$Sepal.Length)
vis_numbygrp <- function(.cat, .num, .labx = "", .laby = "") {
  .cat <- as.character(.cat)
  .cat[is.na(.cat)] <- "NA"
  med <- data.frame(.num, .cat) %>%
    group_by(.cat) %>%
    mutate(Median = median(.num, na.rm = TRUE)) %>%
    pull(Median)

  ggp <- ggplot() +
    geom_violin(aes(reorder(.cat, .num, median, na.rm = TRUE), .num, fill = med), show.legend = FALSE,
                draw_quantiles = 0.5, color = "#555555", na.rm = TRUE) +
    labs(x = .labx, y = .laby) +
    labs() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10))  +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_fill_viridis_c()
  # Return plot
  return(ggp)
}
