#' @include internal.R
NULL

#' Error message
#'
#' Create an error message based on a `try-error` object.
#'
#' @param x `try-error` object.
#'
#' @return `character` value.
#'
#' @export
error_message <- function(x) {
  assertthat::assert_that(inherits(x, "try-error"))
  out <- paste(strsplit(as.character(x), ": ")[[1]][-1], collapse = ": ")
  out <- paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
  out
}
