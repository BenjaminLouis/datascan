#' Bar plot of a categorical column in a dataframe
#'
#' @param .data a dataframe
#' @param .cat unquoted name of a categrocial column
#' @param .by (optional) unquoted name of a categrocial column
#' @param nas logical. Should missing values be considered as group on x-axis
#' @param bytype only if \code{.by} argument is given. Either "count" or "percent" for y-axis
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr pull select count desc
#' @importFrom ggplot2 ggplot aes coord_flip geom_bar labs theme_classic theme element_text scale_y_continuous scale_fill_viridis_d
#' @importFrom rlang enquo quo_name
#' @importFrom stats reorder
#' @importFrom tidyr complete
#'
#' @examples
#' library(dplyr)
#' plot_bar(starwars, eye_color)
plot_bar <- function(.data, .cat, .by, nas = TRUE, bytype = "count") {

  var <- enquo(.cat)

  if (missing(.by)) {
    by <- quo(NULL)
  } else {
    by <- enquo(.by)
  }

  if (is.numeric(pull(.data, !!var))) {stop(".cat should be a categorical column")}

  df <- select(.data, !!var, !!by)

  #N groups
  if (missing(.by)) {
    ngrp <- length(unique(pull(df, !!var)))
  } else {
    ngrp <- length(unique(pull(df, !!by)))
  }

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
    if (missing(.by)) {
      ggp <- df %>%
        count(!!var) %>%
        ggplot(aes(x = reorder(!!var, n), y = n, fill = !!var)) +
        geom_bar(show.legend = FALSE, stat = "identity", color = "#555555") +
        coord_flip() +
        theme_classic() +
        theme(axis.title = element_text(face = "bold", size = 12),
              axis.text.y = element_text(face = "bold", size = textsize),
              axis.text.x = element_text(face = "bold", size = 10))
    } else {
      ggp <- df %>%
        count(!!var, !!by, .drop = FALSE) %>%
        complete(!!var, !!by, fill = list(n = 0))
      if (bytype == "percent") {
        ggp <- ggp %>%
          group_by(!!by) %>%
          mutate(n = n/sum(n)) %>%
          ungroup()
      }
      ggp <- ggp %>%
        ggplot(aes(x = !!by, y = n, fill = !!var)) +
        geom_bar(stat = "identity", color = "#555555", position = "dodge") +
        coord_flip() +
        theme_classic() +
        theme(axis.title = element_text(face = "bold", size = 12),
              axis.text.y = element_text(face = "bold", size = textsize),
              axis.text.x = element_text(face = "bold", size = 10))
    }
  } else {
    if (missing(.by)) {
    ggp <- df %>%
      count(!!var) %>%
      ggplot(aes(x = reorder(!!var, n), y = n, fill = !!var)) +
      geom_bar(show.legend = FALSE, stat = "identity", color = "#555555") +
      theme_classic() +
      theme(axis.title = element_text(face = "bold", size = 12),
            axis.text.x = element_text(face = "bold", size = textsize),
            axis.text.y = element_text(face = "bold", size = 10))
    } else {
      ggp <- df %>%
        count(!!var, !!by, .drop = FALSE) %>%
        complete(!!var, !!by, fill = list(n = 0))
      if (bytype == "percent") {
        ggp <- ggp %>%
          group_by(!!by) %>%
          mutate(n = n/sum(n)) %>%
          ungroup()
      }
      ggp <- ggp %>%
        ggplot(aes(x = !!by, y = n, fill = !!var)) +
        geom_bar(stat = "identity", color = "#555555", position = "dodge") +
        theme_classic() +
        theme(axis.title = element_text(face = "bold", size = 12),
              axis.text.x = element_text(face = "bold", size = textsize),
              axis.text.y = element_text(face = "bold", size = 10))
    }
  }

  ggp <- ggp +
    labs(y = sub("^.", toupper(substr(bytype, 1, 1)), bytype))  +
    scale_x_discrete(na.translate = nas) +
    scale_fill_viridis_d()
  if (bytype == "percent") {
    ggp +
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0), labels = function(x) paste0(x*100, "%"))
  } else {
    ggp +
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0))
  }


}
