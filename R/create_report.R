#' Render EDA report
#'
#' @param .data a data frame
#' @param ... unquoted columns names of variables to be considered as response
#' @param .regroup logical. Should the groups with less than 5 occurences be grouped together.
#' @param output_format see \link{render}
#' @param output_file see \link{render}
#' @param output_dir see \link{render}
#' @param options list. Options for report
#'
#' @return a report at the format specified in output_format
#' @export
#'
#' @importFrom rmarkdown render html_document
#' @importFrom utils browseURL
#' @importFrom ggplot2 quos
#'
#' @examples
#' ##ToDo
create_report <- function(.data, ..., .regroup = TRUE,
                          output_format = html_document(toc = TRUE, toc_depth = 2),
                          output_file = "report.html",
                          output_dir = getwd(),
                          options = report_options()) {

  target <- quos(...)

  # rendering report
  render(
    input = system.file("rmarkdown/report_template.rmd", package = "datascan"),
    output_format = output_format,
    output_file = output_file,
    output_dir = output_dir,
    params = list(dataset = .data, target = target, regroup = .regroup, config = options)
  )

  # Opening report
  browseURL(path.expand(file.path(output_dir, output_file)))

}
