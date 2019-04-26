#' Historgram of a numerical column in a dataframe
#'
#' @param .data a dataframe
#' @param .num a numeric vector
#' @param .bins bins argument for \code{geom_histogram}
#'
#' @return a ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot geom_histogram aes labs theme_classic theme element_text scale_y_continuous scale_fill_viridis_d
#' @importFrom rlang enquo
#'
#' @examples
#' plot_hist(iris, Sepal.Length)
plot_hist <- function(.data, .num, .bins = ".get_bins") {

  var <- enquo(.num)

  # Remove NAs
  df <- select(.data, !!var) %>%
    drop_na()
  # Test if numeric
  if (!is.numeric(pull(df, !!var))) {stop(".num should be a numerical column")}

  bound = pull(df, !!var) %>% min()
  bi <- do.call(.bins, list(.num = pull(df, !!var)))
  # Get the plot
  ggp <- ggplot(df, aes(!!var, fill = ".num")) +
    geom_histogram(show.legend = FALSE, bins = bi,
                   boundary = bound, color = "#555555") +
    labs(x = quo_name(var), y = "Count") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(face = "bold", size = 10))  +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_fill_viridis_d(option = "E")
  # Return plot
  return(ggp)
}







