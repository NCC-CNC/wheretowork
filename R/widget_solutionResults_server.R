#' @include internal.R
NULL

#' Add a solution results
#'
#' Add a solution to the [solutionResults] widget.
#'
#' @param value [Solution] object.
#'
#' @inheritParams updateMapManagerLayer
#'
#' @export
addSolutionResults <- function(
  session = shiny::getDefaultReactiveDomain(), inputId, value) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(inputId),
    assertthat::noNA(inputId),
    inherits(value, "Solution"))
  # pass data to widget
  session$sendCustomMessage(
    "solutionResults:addSolution",
    list(id = inputId, value = value$get_solution_results_widget_data()))
}

#' Drop a solution results
#'
#' Drop a solution to the [solutionResults] widget.
#'
#' @param value `character` identifier for the [Solution] to drop.
#'
#' @inheritParams updateMapManagerLayer
#'
#' @export
dropSolutionResults <- function(
  session = shiny::getDefaultReactiveDomain(), inputId, value) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(inputId),
    assertthat::noNA(inputId),
    assertthat::is.string(value),
    assertthat::noNA(value))
  # pass data to widget
  session$sendCustomMessage(
    "solutionResults:dropSolution", list(id = inputId, value = value))
}

#' Show solution results
#'
#' Show the results for a solution in the [solutionResults] widget.
#'
#' @param value `character` identifier for the [Solution] to show.
#'
#' @inheritParams updateMapManagerLayer
#'
#' @export
showSolutionResults <- function(
  session = shiny::getDefaultReactiveDomain(), inputId, value) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(inputId),
    assertthat::noNA(inputId),
    assertthat::is.string(value),
    assertthat::noNA(value))
  # pass data to widget
  session$sendCustomMessage(
    "solutionResults:showSolution", list(id = inputId, value = value))
}
