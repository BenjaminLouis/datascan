#' Bar plot of a categorical column in a dataframe
#'
#' @param .data a dataframe
#' @param .cat unquoted name of a categrocial column
#' @param .by (optional) unquoted name of a categrocial column
#' @param nas logical. Should missing values be considered as group on x-axis
#' @param type  Either "count" or "percent" for y-axis
#' @param strwidth with of labels (for \code{stringr::str_wrap})
#' @param order (lgl) should the group be ordered by frequency? (chr) Order of .cat groups in barplot
#' @param autoflip (lgl) flip coordinates if there is more than 5 groups
#' @param maxp only if \code{type = "percent"}. Max percentage of y axis
#' @param ystep step for y-axis. \code{ystep} is multiplied by \code(10^(nchar(maxy) - 2)) where maxy is the max of y values
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr pull select count desc group_by ungroup
#' @importFrom ggplot2 ggplot aes coord_flip geom_bar labs theme_classic theme element_text scale_y_continuous scale_fill_viridis_d
#' @importFrom rlang enquo quo_name ':='
#' @importFrom stats reorder
#' @importFrom tidyr complete
#' @importFrom stringr str_wrap
#'
#' @examples
#' library(dplyr)
#' plot_bar(starwars, eye_color)
plot_bar <- function(.data, .cat, .by, nas = TRUE, type = "count", strwidth = 30, order = TRUE, autoflip = TRUE,
                     maxp = 1, ystep = 5) {

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
  if (ngrp > 5 & autoflip) {
    if (missing(.by)) {
      # Count groups
      ggp <- df %>%
        count(!!var)
      # Scale
      maxy <- ggp %>% pull(n) %>% max()
      den <- ystep*10^(nchar(maxy) - 2)
      ymax <- ceiling(maxy/den)*den
      # Percent
      if (type == "percent") {
        ggp <- ggp %>%
          mutate(n = n/sum(n))
      }
      # Order of bars
      if (is.logical(order)) {
        if (order) {
          ggp <- ggp %>%
            mutate(!!var := str_wrap(!!var, strwidth)) %>%
            ggplot(aes(x = reorder(!!var, n), y = n, fill = !!var))
        } else {
          ggp <- ggp %>%
            mutate(!!var := str_wrap(!!var, strwidth)) %>%
            ggplot(aes(x = !!var, y = n, fill = !!var))
        }
      } else if (is.character(order)) {
        ggp <- ggp %>%
          mutate(!!var := factor(str_wrap(!!var, strwidth),
                                 levels = str_wrap(order, strwidth),
                                 ordered = TRUE)) %>%
          ggplot(aes(x = !!var, y = n, fill = !!var))
      } else {
        stop("order argument should be either a logical or a character vector")
      }
      # Plots
      ggp <- ggp +
        geom_bar(show.legend = FALSE, stat = "identity", color = "#555555") +
        coord_flip() +
        theme_classic() +
        theme(axis.title.x = element_text(face = "bold", size = 12),
              axis.title.y = element_blank(),
              axis.text.y = element_text(face = "bold", size = textsize),
              axis.text.x = element_text(face = "bold", size = 10))
    } else {
      ggp <- df %>%
        count(!!var, !!by, .drop = FALSE) %>%
        complete(!!var, !!by, fill = list(n = 0))
      # Scale
      maxy <- ggp %>% pull(n) %>% max()
      den <- ystep*10^(nchar(maxy) - 2)
      ymax <- ceiling(maxy/den)*den
      # Percent
      if (type == "percent") {
        ggp <- ggp %>%
          group_by(!!by) %>%
          mutate(n = n/sum(n)) %>%
          ungroup()
      }
      # Order of bars
      if (is.logical(order)) {
        if (order) {
          ggp <- ggp %>%
            mutate(!!by := str_wrap(!!by, strwidth)) %>%
            ggplot(aes(x = reorder(!!by, desc(n)), y = n, fill = !!var))
        } else {
          ggp <- ggp %>%
            mutate(!!by := str_wrap(!!by, strwidth)) %>%
            ggplot(aes(x = !!by, y = n, fill = !!var))
        }
      } else if (is.character(order)) {
        ggp <- ggp %>%
          mutate(!!by := factor(str_wrap(!!by, strwidth),
                                 levels = str_wrap(order, strwidth),
                                 ordered = TRUE)) %>%
          ggplot(aes(x = !!by, y = n, fill = !!var))
      } else {
        stop("order argument should be either a logical or a character vector")
      }
      # Plots
      ggp <- ggp +
        geom_bar(stat = "identity", color = "#555555", position = "dodge") +
        coord_flip() +
        theme_classic() +
        theme(axis.title.x = element_text(face = "bold", size = 12),
              axis.title.y = element_blank(),
              axis.text.y = element_text(face = "bold", size = textsize),
              axis.text.x = element_text(face = "bold", size = 10))
    }
  #Don't fip coordinates
  } else {
    if (missing(.by)) {
      ggp <- df %>%
        count(!!var)
      # Scale
      maxy <- ggp %>% pull(n) %>% max()
      den <- ystep*10^(nchar(maxy) - 2)
      ymax <- ceiling(maxy/den)*den
      # Percent
      if (type == "percent") {
        ggp <- ggp %>%
          mutate(n = n/sum(n))
      }
      # Order of bars
      if (is.logical(order)) {
        if (order) {
          ggp <- ggp %>%
            mutate(!!var := str_wrap(!!var, strwidth)) %>%
            ggplot(aes(x = reorder(!!var, desc(n)), y = n, fill = !!var))
        } else {
          ggp <- ggp %>%
            mutate(!!var := str_wrap(!!var, strwidth)) %>%
            ggplot(aes(x = !!var, y = n, fill = !!var))
        }
      } else if (is.character(order)) {
        ggp <- ggp %>%
          mutate(!!var := factor(str_wrap(!!var, strwidth),
                                 levels = str_wrap(order, strwidth),
                                 ordered = TRUE)) %>%
          ggplot(aes(x = !!var, y = n, fill = !!var))
      } else {
        stop("order argument should be either a logical or a character vector")
      }
      # Plots
      ggp <- ggp +
      geom_bar(show.legend = FALSE, stat = "identity", color = "#555555") +
      theme_classic() +
      theme(axis.title.y = element_text(face = "bold", size = 12),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = textsize),
            axis.text.y = element_text(face = "bold", size = 10))
    } else {
      # Count groups
      ggp <- df %>%
        count(!!var, !!by, .drop = FALSE) %>%
        complete(!!var, !!by, fill = list(n = 0))
      # Scale
      maxy <- ggp %>% pull(n) %>% max()
      den <- ystep*10^(nchar(maxy) - 2)
      ymax <- ceiling(maxy/den)*den
      # Percent
      if (type == "percent") {
        ggp <- ggp %>%
          group_by(!!by) %>%
          mutate(n = n/sum(n)) %>%
          ungroup()
      }
      # Order of bars
      if (is.logical(order)) {
        if (order) {
          ggp <- ggp %>%
            mutate(!!by := str_wrap(!!by, strwidth)) %>%
            ggplot(aes(x = reorder(!!by, desc(n)), y = n, fill = !!var))
        } else {
          ggp <- ggp %>%
            mutate(!!by := str_wrap(!!by, strwidth)) %>%
            ggplot(aes(x = !!by, y = n, fill = !!var))
        }
      } else if (is.character(order)) {
        ggp <- ggp %>%
          mutate(!!by := factor(str_wrap(!!by, strwidth),
                                levels = str_wrap(order, strwidth),
                                ordered = TRUE)) %>%
          ggplot(aes(x = !!by, y = n, fill = !!var))
      } else {
        stop("order argument should be either a logical or a character vector")
      }
      # plots
      ggp <- ggp +
        geom_bar(stat = "identity", color = "#555555", position = "dodge") +
        theme_classic() +
        theme(axis.title.y = element_text(face = "bold", size = 12),
              axis.title.x = element_blank(),
              axis.text.x = element_text(face = "bold", size = textsize),
              axis.text.y = element_text(face = "bold", size = 10))
    }
  }

  ggp <- ggp +
    labs(y = sub("^.", toupper(substr(type, 1, 1)), type))  +
    scale_x_discrete(na.translate = nas) +
    scale_fill_viridis_d()
  if (type == "percent") {
    ggp +
      scale_y_continuous(limits = c(0, maxp),
                         breaks = seq(0, maxp, 0.2),
                         expand = c(0, 0),
                         labels = function(x) paste0(x*100, "%"))
  } else {
    ggp +
      scale_y_continuous(limits = c(0, ymax), #max(c(ymax, (maxy + den)))),
                         breaks = seq(0, ymax, den),
                         expand = c(0, 0))
  }


}
