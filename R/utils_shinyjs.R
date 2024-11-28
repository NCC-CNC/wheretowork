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

#' Change file icon and update tool tip
#'
#' @param x `character` CSS selector.
#'
#' @param hex `character` hex color.
#' 
#' @param tool_tip `character` description text.
#' 
#' @return Invisible `TRUE`.
#'
#' @return Invisible `TRUE`.
#'
#' @export
change_file_icon_js <- function(x, hex, tool_tip) {
  shinyjs::runjs(
    paste0(
      "document.querySelector('", x, "').style.color = '", hex, "';",
      "$(document.querySelector('", x, "'))
        .attr('title', '", tool_tip, "')
        .tooltip('fixTitle');"
    )
  )
  invisible(TRUE)
}

#' Remove map pane using CSS selector
#'
#' @param id `character` CSS selector.
#'
#' @return Invisible `TRUE`.
#'
#' @export
removeMapPane <- function(id) {
  shinyjs::runjs(
    paste0(
      "document.querySelector('.leaflet-pane-", id, "-pane').remove();"
    )
  )
  invisible(TRUE)
}