
vis_groups <- function(.data) {
  # Select cat columns
  catdata <- select_if(.data, ~!is.numeric(.x) & !is.list(.x))
  # There should be numeric colmns
  if (ncol(catdata) == 0) {stop("No categorical columns found")}
  # Get the distr
  ggplist <- mapply(vis_grp, .cat = catdata, .labx = names(catdata), SIMPLIFY = FALSE)
  # return the distr
  return(ggplist)
}