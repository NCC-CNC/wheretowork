#' @include internal.R
NULL

#' Clean import settings widget
#'
#' Remove all field/layers displayed in the widget.
#'
#' @param session The `session` object passed to function given to
#'   `shinyServer` Default is [shiny::getDefaultReactiveDomain()].
#'
#' @param inputId `character` The identifier of the input object.
#'
#' @seealso [importSettings()].
#'
#' @export
cleanImportSettings <- function(
  session = shiny::getDefaultReactiveDomain(), inputId) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(inputId),
    assertthat::noNA(inputId))

  # pass data to widget
  session$sendCustomMessage(
    "importSettings:clean", list(id = inputId))
}
