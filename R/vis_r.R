#' Plot R coefficient of linear models between numeric and catagorical columns in a dataframe
#'
#' @param .data a data frame
#'
#' @return a gpplot
#' @export
#'
#' @importFrom ggplot2 ggplot aes sym geom_tile theme_classic theme element_blank element_text scale_fill_viridis_d scale_y_discrete scale_x_discrete labs
#'
#' @examples
#' vis_r(dplyr::starwars)
vis_r <- function(.data) {

  result <- scan_r(.data)
  wh <- with(result, which.max(c(length(unique(Num)), length(unique(Cat)))))
  xvar <- c("Num", "Cat")[-wh]
  yvar <- c("Num", "Cat")[wh]

  ggplot(result, aes(!!sym(xvar), !!sym(yvar), fill = cut(r, seq(0, 1, 0.2),
                                                labels = c("0 - 0.2",
                                                           "0.2 - 0.4",
                                                           "0.4 - 0.6",
                                                           "0.6 - 0.8",
                                                           "0.8 - 1"),
                                                ordered_result = TRUE))) +
    geom_tile() +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 10),
          legend.position = "bottom")  +
    scale_fill_viridis_d(option = "D") +
    scale_y_discrete(expand = c(0, 0),
                     limits = sort(unique(result[[yvar]]), decreasing = TRUE)) +
    scale_x_discrete(expand = c(0, 0), position = "top",
                     limits = sort(unique(result[[xvar]]), decreasing = FALSE)) +
    labs(fill = "R")
}



