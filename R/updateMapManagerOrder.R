#' @include internal.R SolutionSettings-class.R solutionSettings.R
NULL

#' Update map manager widget ordering
#'
#' Change the order of the layers for a map manager widget on the client.
#'
#' @param session The `session` object passed to function given to
#'   `shinyServer` Default is [shiny::getDefaultReactiveDomain()].
#'
#' @param inputId `character` The identifier of the input object.
#'
#' @param value `numeric` Vector containing the new ordering.
#'
#' @seealso [mapManager()].
#'
#' @export
updateMapManagerOrder <- function(
  session = shiny::getDefaultReactiveDomain(), inputId, value) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(inputId),
    assertthat::noNA(inputId),
    is.numeric(value),
    assertthat::noNA(value),
    identical(anyDuplicated(value), 0L))

  # pass data to widget
  session$sendCustomMessage(
    "mapManager:updateOrder", list(id = inputId, value = value))
}
