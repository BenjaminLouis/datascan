#' Gives some statistics about numeric data distributions
#'
#' @param data a data.frame
#'
#' @return a data.frame with one row for each numeric variables in \code{data} with
#' minimum (\code{Min}) and maximum (\code{Max}) values, 1st, 2nd
#' and 3rd quartiles (\code{Q1}, \code{Q2}, \code{Q3}), interquartile range value (IQR), median absolute deviation
#' from the median (\code{MADM}), MADM to median ratio (\code{MADM/Q2}) and skewness
#'
#' @export
#'
#' @importFrom dplyr select_if
#' @importFrom stats quantile IQR median
#'
#' @examples
#' data(iris)
#' scan_distributions(iris)
scan_distributions <- function(data) {

  # Selection of numeric variables
  numdata <- select_if(data, is.numeric)
  # There should be numeric variables
  if (ncol(numdata) == 0) {stop("No numeric variables found")}

  #Stats
  qu <-  sapply(numdata, quantile, probs = seq(0, 1, 0.25), na.rm = TRUE)
  iqr <-  sapply(numdata, IQR, na.rm = TRUE)
  madm <- sapply(numdata, .get_madm)
  rmadm <- sapply(numdata, function(x) .get_madm(x)/median(x, na.rm = TRUE))
  skewness <- sapply(numdata, .get_skewness)

  #As dataframe
  dstats <- as.data.frame(t(rbind.data.frame(qu, iqr, madm, rmadm, skewness)))
  vars <- data.frame(Var = colnames(qu))
  dstats <- cbind.data.frame(vars, dstats)
  colnames(dstats) <- c("Var","Min", "Q1", "Q2", "Q3", "Max", "IQR", "MADM", "MADM/Q2", "Skewness")

  #return
  return(dstats)

}
