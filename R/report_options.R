#' Config options for automatic report
#'
#' @param .title report title
#' @param .author report author
#' @param .date report date
#' @param .regroup logical. Should the groups with less than 5 occurences be grouped together.
#' @param .target columns names of variables to be considered as response.
#'
#' @return a list of config values
#' @export
report_options <- function(.title = "Exploratory Data Analysis report",
                           .author = "John Doe", .date = Sys.Date(),
                           .regroup = TRUE, .target = NULL) {
  list(title = .title, author = .author, date = .date)
}
