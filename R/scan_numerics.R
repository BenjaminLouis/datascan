#' Summary statistics about numerical columns in a dataframe
#'
#' @param .data a dataframe
#'
#' @return a data.frame with one row for each numeric variables in \code{data} with
#' minimum (\code{Min}) and maximum (\code{Max}) values, 1st, 2nd
#' and 3rd quartiles (\code{Q1}, \code{Med}, \code{Q2}), interquartile range value (IQR), median absolute deviation
#' from the median (\code{MADM}), MADM to median ratio (\code{MADM/Q2}) and skewness
#'
#' @export
#'
#' @importFrom dplyr select_if group_by mutate rename select distinct
#' @importFrom stats quantile IQR
#' @importFrom tidyr gather unnest
#'
#' @examples
#' scan_numerics(iris)
scan_numerics <- function(.data) {

  # Selection of numeric variables
  numdata <- select_if(.data, is.numeric)
  # There should be numeric variables
  if (ncol(numdata) == 0) {stop("There should be numerical columns in .data")}

  numdata %>%
    gather(key = "Variable", value = "value") %>%
    group_by(Variable) %>%
    mutate(qu = list(as.data.frame(t(quantile(value, probs = seq(0, 1, 0.25), na.rm = TRUE))))) %>%
    unnest(qu) %>%
    rename(Min = '0%', Max = '100%', Med = '50%', Q1 = '25%', Q3 = '75%') %>%
    mutate(IQR = IQR(value, na.rm = TRUE)) %>%
    mutate(MADM = .get_madm(value)) %>%
    mutate('MADM/Med' = MADM/Med) %>%
    mutate(Skewness = .get_skewness(value)) %>%
    select(-value) %>%
    distinct()

}


# scan_numerics <- function(.data) {
#
#   # Selection of numeric variables
#   numdata <- select_if(.data, is.numeric)
#   # There should be numeric variables
#   if (ncol(numdata) == 0) {stop("here should be numerical columns in .data")}
#
#   #Stats
#   qu <-  sapply(numdata, quantile, probs = seq(0, 1, 0.25), na.rm = TRUE)
#   iqr <-  sapply(numdata, IQR, na.rm = TRUE)
#   madm <- sapply(numdata, .get_madm)
#   rmadm <- sapply(numdata, function(x) .get_madm(x)/median(x, na.rm = TRUE))
#   skewness <- sapply(numdata, .get_skewness)
#
#   #As dataframe
#   dstats <- as.data.frame(t(rbind.data.frame(qu, iqr, madm, rmadm, skewness)))
#   vars <- data.frame(Var = colnames(qu))
#   dstats <- cbind.data.frame(vars, dstats)
#   colnames(dstats) <- c("Var","Min", "Q1", "Q2", "Q3", "Max", "IQR", "MADM", "MADM/Q2", "Skewness")
#
#   #return
#   return(dstats)
#
# }
