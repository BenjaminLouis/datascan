#' plots correlation between numerical columns of a dataframe
#'
#' @param .data a dataframe
#' @param .use see \link[stats]{cor}
#' @param .method see \link[stats]{cor}
#'
#' @return a ggplot
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile theme_classic theme element_blank element_text scale_fill_gradient2 scale_y_discrete scale_x_discrete labs
#'
#' @examples
#' vis_corr(iris)
vis_corr <- function(.data, .use = "pairwise.complete.obs", .method = "spearman") {

  result <- scan_corr(.data, .use, .method)

  ggplot(result, aes(Num1, Num2, fill = Corr)) +
    geom_tile(color = "#555555") +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 10),
          legend.position = "right",
          legend.title = element_text(face = "bold", size = 12))  +
    #scale_fill_viridis_c(option = "E", limits = c(-1, 1), alpha = 0.8) +
    scale_fill_gradient2(low = "#00204DFF", mid = "#EAEDE9", high = "#BB3754FF",
                         breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1, 1),
                         na.value = "gray100") +
    scale_y_discrete(expand = c(0, 0),
                     limits = sort(unique(result$Num1), decreasing = TRUE)) +
    scale_x_discrete(expand = c(0, 0), position = "top",
                     limits = sort(unique(result$Num2), decreasing = FALSE)) +
    labs(fill = paste0(gsub("^.", toupper(substr(.method, 1, 1)) , .method),
                       "'s\nCorrelation"))

}
