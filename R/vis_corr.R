#' Visualise correlation between numeric columns of a dataframe
#'
#' @param .data a dataframe
#' @param .use see \link[stats]{cor}
#' @param .method see \link[stats]{cor}
#'
#' @return a ggplot
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile theme_classic theme element_blank element_text scale_fill_viridis_d scale_y_discrete scale_x_discrete labs
#'
#' @examples
#' vis_corr(iris)
vis_corr <- function(.data, .use = "na.or.complete", .method = "spearman") {

  result <- scan_corr(.data, .use, .method)

  ggplot(result, aes(Num1, Num2, fill = cut(Corr, seq(-1, 1, 0.2),
                                            labels = c("[-1:-0.8]",
                                                       "[-0.8:-0.6]",
                                                       "[-0.6:-0.4]",
                                                       "[-0.4:-0.2]",
                                                       "[-0.2:0]",
                                                       "[0:0.2]",
                                                       "[0.2:0.4]",
                                                       "[0.4:0.6]",
                                                       "[0.6:0.8]",
                                                       "[0.8:1]"),
                                            ordered_result = TRUE,
                                            include.lowest = TRUE,
                                            right = FALSE))) +
    geom_tile(color = "#555555") +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 10),
          legend.position = "right")  +
    scale_fill_viridis_d(option = "D", drop = FALSE) +
    scale_y_discrete(expand = c(0, 0),
                     limits = sort(unique(result$Num1), decreasing = TRUE)) +
    scale_x_discrete(expand = c(0, 0), position = "top",
                     limits = sort(unique(result$Num2), decreasing = FALSE)) +
    labs(fill = paste0(gsub("^.", toupper(substr(.method, 1, 1)) , .method),
                       "'s\nCorrelation"))

}
