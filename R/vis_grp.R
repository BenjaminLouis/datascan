#' Ggplot of group frequency in a categorical variable
#'
#' @param .cat a categorical vector
#' @param .labx labels of x-axis
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr desc
#' @importFrom ggplot2 ggplot aes coord_flip geom_bar labs theme_classic theme element_text scale_y_continuous scale_fill_viridis_d
#' @importFrom stats reorder
#'
#' @examples
#' library(dplyr)
#' starwars %>%
#'  pull(eye_color) %>%
#'  vis_grp()
vis_grp <- function(.cat, .labx = "") {

  if (is.numeric(.cat)) {stop(".cat should not be a numeric vector")}

  df <- as.data.frame(table(.cat))
  names(df) <- c("varia", "Freq")

  # Flip coordinate if there is more than 5 groups
  if (length(unique(.cat)) > 5) {
    ggp <- ggplot(df, aes(x = reorder(varia, Freq), y = Freq, fill = varia)) +
      coord_flip()
  } else {
    ggp <- ggplot(df, aes(x = reorder(varia, desc(Freq)), y = Freq, fill = varia))
  }

  ggp <- ggp +
    geom_bar(show.legend = FALSE,
             stat = "identity",
             color = "#555555") +
    labs(x = .labx, y = "Count") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10))  +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_fill_viridis_d()

  # Return plot
  return(ggp)

}
