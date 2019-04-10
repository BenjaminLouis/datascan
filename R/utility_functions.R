#' Some internals functions
#' @param .data a data frame
#' @param .num a numeric vector
#' @param .cat a categorical vector
#' @param .cat1 a categorical vector
#' @param .cat2 a categorical vector
#' @name utils
NULL

#' @rdname utils
#' @importFrom stats median
#' @export
#' @examples
#' .get_madm(rnorm(100))
.get_madm <- function(.num) {
  median(abs(.num - median(.num, na.rm = TRUE)), na.rm = TRUE)
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
#' .get_bins(rnorm(100))
.get_bins <- function(.num) {
  .num <- .num[!is.na(.num)]
  floor(1.5*length(.num)/10) %>% min(30) %>% max(10)
}

#' @rdname utils
#' @importFrom stats chisq.test
#' @export
#' @examples
#' #todo
.get_cramerv <- function(.cat1, .cat2) {
  # To consider NA's as a group
  .cat1 <- factor(.cat1)
  levels(.cat1) <- c(levels(.cat1), "Missing")
  .cat1[is.na(.cat1)] <- "Missing"
  .cat2 <- factor(.cat2)
  levels(.cat2) <- c(levels(.cat2), "Missing")
  .cat2[is.na(.cat2)] <- "Missing"
  #This used bias corrected Cramer's V
  # https://en.wikipedia.org/wiki/Cram%C3%A9r%27s_V#Bias_correction
  n <- length(.cat1)
  k <- length(unique(.cat1))
  r <- length(unique(.cat2))
  # chisq simulate.p.value = TRUE to avoir warning about p-value estimation
  # but B = 1 for time optimisation because I don't care about p-value
  phi2 <- chisq.test(.cat1, .cat2, simulate.p.value = TRUE, B = 1)$statistic/n
  phi2cor <- max(0, phi2 - ((k - 1) * (r - 1)) / (n - 1))
  kcor <- k - (((k - 1)^2)/(n - 1))
  rcor <- r - (((r - 1)^2)/(n - 1))
  cramerV <- sqrt(phi2cor/min(kcor - 1, rcor - 1))
  return(cramerV)
}

#' @rdname utils
#' @importFrom stats lm as.formula na.omit
#' @export
#' @examples
#' #todo
.get_r <- function(.data, .cat, .num) {
  mod <- lm(formula = as.formula(paste0(.num, "~", .cat)), data = .data, na.action = na.omit)
  sqrt(summary(mod)$r.squared)
}

#' @rdname utils
#' @importFrom stats setNames
#' @importFrom dplyr recode
#' @export
#' @examples
#' library(dplyr)
#' .transform_cat(starwars$species)
#' .transform_cat(starwars$gender)
.transform_cat <- function(.cat) {
  tt <- table(.cat, useNA = "ifany")
  ok <- names(tt[tt >= 5])
  nas <- if (any(is.na(ok))) {NA_character_} else {"Other"}
  ok <- ok[!is.na(ok)]
  reco <- setNames(ok, ok)
  recode(.cat, !!!reco, .default = "Others groups", .missing = nas)
}
