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
cleanImportSettings <- function(session = shiny::getDefaultReactiveDomain(),
                                inputId) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(inputId),
    assertthat::noNA(inputId)
  )

  # pass data to widget
  session$sendCustomMessage(
    "importSettings:clean", list(id = inputId)
  )
}

#' Update import settings widget
#'
#' Change the field/layers displayed in the widget.
#'
#' @param session The `session` object passed to function given to
#'   `shinyServer` Default is [shiny::getDefaultReactiveDomain()].
#'
#' @param inputId `character` The identifier of the input object.
#'
#' @param value `character` Vector containing the new field/layers.
#'
#' @seealso [importSettings()].
#'
#' @export
updateImportSettings <- function(session = shiny::getDefaultReactiveDomain(),
                                 inputId, value) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(inputId),
    assertthat::noNA(inputId),
    is.character(value),
    assertthat::noNA(value),
    identical(anyDuplicated(value), 0L)
  )
  # pass data to widget
  session$sendCustomMessage(
    "importSettings:update", list(id = inputId, value = value)
  )
}
