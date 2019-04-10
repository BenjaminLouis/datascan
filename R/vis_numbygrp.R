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
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10))  +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_fill_viridis_c()
  # Return plot
  return(ggp)
}
