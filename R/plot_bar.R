#' Bar plot of a categorical column in a dataframe
#'
#' @param .data a dataframe
#' @param .cat unquoted name of a categrocial column
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr pull select count desc
#' @importFrom ggplot2 enquo ggplot aes coord_flip geom_bar labs quo_name theme_classic theme element_text scale_y_continuous scale_fill_viridis_d
#' @importFrom stats reorder
#'
#' @examples
#' library(dplyr)
#' plot_bar(starwars, eye_color)
plot_bar <- function(.data, .cat) {

  var <- enquo(.cat)

  if (is.numeric(pull(.data, !!var))) {stop(".cat should be a categorical column")}

  df <- select(.data, !!var) %>%
    count(!!var)

  #N groups
  ngrp <- length(unique(pull(df, !!var)))
  if (ngrp <= 30) {
    textsize <- 10
  } else if (ngrp > 30 & ngrp <= 40) {
    textsize <- 8
  } else if (ngrp > 40 & ngrp <= 50) {
    textsize <- 7
  } else {
    textsize <- 5
  }

  #Flip coordinate if there is more than 5 groups
  if (ngrp > 5) {
    ggp <- ggplot(df, aes(x = reorder(!!var, n), y = n, fill = !!var)) +
      coord_flip() +
      theme_classic() +
      theme(axis.title = element_text(face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = textsize),
            axis.text.x = element_text(face = "bold", size = 10))
  } else {
    ggp <- ggplot(df, aes(x = reorder(!!var, desc(n)), y = n, fill = !!var)) +
      theme_classic() +
      theme(axis.title = element_text(face = "bold", size = 12),
            axis.text.x = element_text(face = "bold", size = textsize),
            axis.text.y = element_text(face = "bold", size = 10))
  }

  ggp +
    geom_bar(show.legend = FALSE,
             stat = "identity",
             color = "#555555") +
    labs(x = quo_name(var), y = "Count")  +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_fill_viridis_d()


}
