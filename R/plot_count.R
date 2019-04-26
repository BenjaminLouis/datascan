#' Count overlapping points of two categorical columns in a dataframe
#'
#' @param .data a dataframe
#' @param .cat1 a categorical column
#' @param .cat2 a categorical column
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr select mutate_all funs pull
#' @importFrom ggplot2 ggplot aes stat geom_count scale_size_area theme_classic theme element_text unit element_rect scale_color_viridis_c scale_y_discrete scale_x_discrete labs
#' @importFrom rlang enquo
#' @importFrom purrr map_chr
#' @importFrom tidyr drop_na
#'
#' @examples
#' library(dplyr)
#' plot_count(starwars, eye_color, hair_color)
plot_count <- function(.data, .cat1, .cat2) {

  varx <- enquo(.cat1)
  vary <- enquo(.cat2)

  # Transform NA as a group to avoid warning
  df <- select(.data, !!varx, !!vary) %>%
    mutate_all(funs(map_chr(., ~if (is.na(.x)) {"NA"} else {.x})))

  limx <- sort(unique(pull(df, !!varx)))
  limy <- sort(unique(pull(df, !!vary)), decreasing = TRUE)

  ggplot(df, aes(!!varx, !!vary, color = stat(prop), group = 1)) +
    geom_count()  +
    scale_size_area(max_size = 10, guide = FALSE) +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.key.size = unit(0.8, "cm"),
          panel.border = element_rect(color = "black", fill = "transparent"))  +
    scale_color_viridis_c(option = "D") +
    scale_y_discrete(limits = limy) +
    scale_x_discrete(position = "top", limits = limx) +
    labs(color = "Proportion\n")

}
