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

#' Enable a HTML element using a CSS selector
#'
#' Enable a HTML element using a CSS selector.
#'
#' @param x `character` CSS selector.
#'
#' @return Invisible `TRUE`.
#'
#' @export
enable_html_css_selector <- function(x) {
  shinyjs::runjs(
    paste0(
      "document.querySelector('", x, "').classList.remove('disabled');"
    )
  )
  invisible(TRUE)
}

#' Disable HTML element(s) using a CSS selector
#'
#' Disable HTML element(s) using a CSS selector.
#'
#' @param x `character` CSS selector.
#'
#' @return Invisible `TRUE`.
#'
#' @return Invisible `TRUE`.
#'
#' @export
disable_html_css_selector <- function(x) {
  shinyjs::runjs(
    paste0(
      "document.querySelector('", x, "').classList.add('disabled');"
    )
  )
  invisible(TRUE)
}

#' Change file icon to red, update tool tip; fileinput_parameter
#'
#' @param x `character` CSS selector.
#'
#' @return Invisible `TRUE`.
#'
#' @return Invisible `TRUE`.
#'
#' @export
change_file_icon_js <- function(x) {
  shinyjs::runjs(
    paste0(
      "document.querySelector('", x, "').style.color = '", "#f03b20", "';",
      "$(document.querySelector('", x, "'))
        .attr('title', 'Solution settings did not update.')
        .tooltip('fixTitle');"
    )
  )
  invisible(TRUE)
}
