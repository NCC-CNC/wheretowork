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
  assertthat::assert_that(inherits(x, c("try-error", "error")))
  out <- paste(strsplit(as.character(x), ": ")[[1]][-1], collapse = ": ")
  out <- paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
  out
}

#' Error log
#'
#' Create an error log based on a `try-error` object.
#'
#' @param x `try-error` object.
#'
#' @details
#' This function checks `attr(x, "log")` for a list of `SimpleError` objects
#' that are used to construct the log. If the argument to `x` does not
#' contain this attribute, then the text `"No error log available."`
#' is returned.
#'
#' @return `character` value.
#'
#' @export
error_log <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, c("try-error", "error")))
  # extract log
  logs <- attr(x, "log")
  # format log
  if (is.list(logs)) {
    ## prepare log error
    out <- c(
      "# wheretowork project import log",
      "",
      "## Summary",
      "",
      error_message(x),
      "## Details",
      "",
      unlist(
        recursive = TRUE, use.names = FALSE,
        lapply(logs, function(x) {
          c(paste("###", x$name), "", as.character(x$details))
        })
      )
    )
  } else {
    out <- c(
     "# wheretowork project import log",
      "",
      "## Summary",
      "",
      as.character(x),
      "",
      "## Details",
      "",
      "Sorry, no further details available."
    )
  }
  # return log
  out
}
