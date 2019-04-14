#' Some internals functions
#' @param x a numeric vector
#' @param nr a numeric value
#' @param color css color
#' @param simple a logical
#' @name utils-style
NULL

#' @rdname utils-style
#' @export
.prop <- function(x, nr) {paste0(round(100*x/nr, 2), "%")}

#' @rdname utils-style
#' @export
.color_bar <- function(x, nr, color = "lightgreen", simple = TRUE) {
  if (simple) {
    sapply(x, function(a) {
      paste0("<span style = \"display: block; direction: rtl; border-radius: 4px; width: 100%; border: 1px solid ", color, ";\">", a,"</span>")})
  } else {
    sapply(x, function(a) {
      paste0("<span style = \"display: block; direction: rtl; border-radius: 4px; width: 100%; border: 1px solid ", color, ";\">",
             paste0("<span style = \"display: block; direction: rtl; border-radius: 2px; width: ", .prop(as.numeric(a), nr), "; background-color: ", color, ";\">", a,"</span>"),
             "</span>")})
  }
}

#' @rdname utils-style
#' @export
.apply_colorbar <- function(x, nr) {
  x[!is.na(x) & as.character(x) != "0"] <- .color_bar(x[!is.na(x) & as.character(x) != "0"], simple = FALSE, nr = nr)
  x[!is.na(x) & as.character(x) == "0"] <- .color_bar(x[!is.na(x) & as.character(x) == "0"], nr = nr)
  return(x)
}
