#' Plot R coefficient of linear models between numeric and catagorical columns in a dataframe
#'
#' @param .data a data frame
#'
#' @return a gpplot
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile theme_classic theme element_blank element_text scale_fill_gradient2 scale_y_discrete scale_x_discrete labs
#' @importFrom rlang sym
#'
#' @examples
#' library(dplyr)
#' vis_r(starwars)
vis_r <- function(.data) {

  result <- scan_r(.data)
  wh <- with(result, which.max(c(length(unique(Num)), length(unique(Cat)))))
  xvar <- c("Num", "Cat")[-wh]
  yvar <- c("Num", "Cat")[wh]

  ggplot(result, aes(!!sym(xvar), !!sym(yvar), fill = r)) +
    geom_tile() +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 10),
          axis.text.x = element_text(angle = 45, hjust = -0.01),
          legend.position = "right",
          legend.title = element_text(face = "bold", size = 12))  +
    scale_fill_gradient2(low = "#EAEDE9", mid = "gold2", high = "#601200",
                         breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1),
                         midpoint = 0.5, na.value = "grey")  +
    #scale_fill_viridis_d(option = "D") +
    scale_y_discrete(expand = c(0, 0),
                     limits = sort(unique(result[[yvar]]), decreasing = TRUE)) +
    scale_x_discrete(expand = c(0, 0), position = "top",
                     limits = sort(unique(result[[xvar]]), decreasing = FALSE)) +
    labs(fill = "R")
}



