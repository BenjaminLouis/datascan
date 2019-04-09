vis_grp <- function(.cat, .labx = "") {

  if (is.numeric(.cat)) {stop(".cat should not be a numeric vector")}

  df <- as.data.frame(table(.cat))
  names(df) <- c("var", "Freq")

  ggp <- ggplot(df, aes(x = reorder(var, Freq), y = Freq, fill = var)) +
    geom_bar(show.legend = FALSE,
             stat = "identity") +
    labs(x = .labx, y = "Count") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10))  +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_fill_viridis_d()

  if (length(unique(.cat)) > 5) {
    ggp <- ggp +
      coord_flip()
  }

  # Return plot
  return(ggp)

}
