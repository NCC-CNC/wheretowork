#' @include internal.R
NULL

#' Enable a HTML element
#'
#' Enable a HTML element.
#'
#' @param inputId HTML element id.
#'
#' @return Invisible `TRUE`.
#'
#' @export
enable_html_element <- function(inputId) {
  shinyjs::runjs(
    paste0(
      "document.getElementById(\"",
      inputId,
      "\").removeAttribute(\"disabled\");"
    )
  )
  invisible(TRUE)
}

#' Disable a HTML element
#'
#' Disable a HTML element.
#'
#' @param inputId HTML element id.
#'
#' @return Invisible `TRUE`.
#'
#' @export
disable_html_element <- function(inputId) {
  shinyjs::runjs(
    paste0(
      "document.getElementById(\"",
      inputId,
      "\").setAttribute(\"disabled\", \"\")"
    )
  )
  invisible(TRUE)
}
