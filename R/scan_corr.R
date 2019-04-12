#' Correlation between numerical columns of a dataframe
#'
#' @param .data a dataframe
#' @param .use see \link[stats]{cor}
#' @param .method see \link[stats]{cor}
#'
#' @return a data frame with correlation values of all pairs of numerical columns
#' @export
#'
#' @importFrom dplyr select_if
#' @importFrom stats cor
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#'
#' @examples
#' scan_corr(iris)
scan_corr <- function(.data, .use = "na.or.complete", .method = "spearman") {

  numdata <- select_if(.data, is.numeric)
  # There should be at least 2 numeric variables
  if (ncol(numdata) < 2) {stop("There should be at least 2 numerical columns in .data")}

  cor(numdata, use = .use, method = .method) %>%
    as.data.frame() %>%
    rownames_to_column("Num1") %>%
    gather(key = "Num2", value = "Corr", -Num1)

}
