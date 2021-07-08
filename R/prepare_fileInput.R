#' @include internal.R
NULL

#' Prepare file input
#'
#' Prepare files uploaded using [shiny::fileInput].
#'
#' @param x `data.frame` object.
#'
#' @details
#' This function renames files to their original file names after
#' they have been uploaded.
#'
#' @return `character` file paths.
#'
#' @export
prepare_fileInput <- function(x) {
  assertthat::assert_that(is.data.frame(x))
  path <- file.path(dirname(x$datapath), x$name)
  file.rename(x$datapath, path)
  path
}
