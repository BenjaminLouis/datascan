#' Some internals functions
#' @param .num a numeric vector
#' @name utils
NULL

#' @rdname utils
#' @importFrom stats median
#' @export
#' @examples
#' .get_madm(rnorm(100))
.get_madm <- function(.num) {
  stats::median(abs(.num - stats::median(.num, na.rm = TRUE)), na.rm = TRUE)
}

#' @rdname utils
#' @export
#' @examples
#' .get_skewness(rnorm(100))
.get_skewness <- function(.num) {
  .num <- .num[!is.na(.num)]
  n <- length(.num)
  ((1/n)*sum((.num - mean(.num))^3))/((1/n)*sum((.num - mean(.num))^2))^(3/2)
}


#' @rdname utils
#' @export
#' @examples
#' .get_bins()
get_bins <- function(.num) {
  .num <- .num[!is.na(.num)]
  floor(1.5*length(.num)/10) %>% min(30) %>% max(10)
}




