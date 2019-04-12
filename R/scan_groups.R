#' Frequency of groups in all categorical columns of a dataframe
#'
#' @param .data a data.frame
#'
#' @return a data.frame with one row for each group in each categorical variables
#' in \code{data} with \code{variables}, the categorical varibales names, \code{groups}
#' the groups names of categorical variables, \code{n} the occurence number for each
#' groups and \code{p} the proportion
#'
#' @export
#'
#' @importFrom dplyr select_if group_by count mutate
#' @importFrom tidyr gather
#'
#' @examples
#' data(iris)
#' scan_groups(iris)
scan_groups <- function(.data) {

  # Selection of numeric variables
  catdata <- select_if(.data, ~is.character(.x) | is.factor(.x))
  # There should be numeric variables
  if (ncol(catdata) == 0) {stop("There should be categorical column in .data")}

  #Stats
  catdata %>%
    gather(key = "variables", value = "groups") %>%
    group_by(variables) %>%
    count(groups, sort = TRUE) %>%
    mutate(p = n/sum(n))

}
