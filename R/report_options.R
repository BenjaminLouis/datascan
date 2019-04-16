#' Config options for automatic report
#'
#' @param .title report title
#' @param .author report author
#' @param .date report date
#'
#' @return a list of config values
#' @export
report_options <- function(.title = "Exploratory Data Analysis report",
                           .author = "John Doe", .date = Sys.Date()) {
  config_report <<- list(title = .title, author = .author, date = .date)
}
