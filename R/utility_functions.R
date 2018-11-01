#' Some internals functions
#'
#' @param x a numeric vector
#'
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#' .get_madm(rnorm(100))
.get_madm <- function(x) {
  stats::median(abs(x - stats::median(x, na.rm = TRUE)), na.rm = TRUE)
}

#' @rdname .get_madm
#'
#' @param x a numeric vector
#'
#' @export
#'
#' @examples
#' .get_skewness(rnorm(100))
.get_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  ((1/n)*sum((x - mean(x))^3))/((1/n)*sum((x - mean(x))^2))^(3/2)
}
