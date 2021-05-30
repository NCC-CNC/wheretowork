#' @include internal.R SolutionSettings-class.R solutionSettings.R
NULL

#' Update a layer in a map manager widget
#'
#' Change the layer for a map manager widget on the client.
#'
#' @param session The `session` object passed to function given to
#'   `shinyServer` Default is [shiny::getDefaultReactiveDomain()].
#'
#' @param inputId `character` The identifier of the input object.
#'
#' @param value `list` Object containing the new settings (see Details).
#'
#' @details
#' ## Overview
#'
#' The argument to `value` should be `list` object containing
#' the information necessary to update a layer.
#' Broadly speaking, it should contain the following elements:
#'
#' \describe{
#' \item{id}{`character` value with the identifier for the layer.}
#' \item{parameter}{`character` value with the name of the parameter to update.}
#' \item{value}{`ANY` new value for the parameter.}
#' }
#'
#' Note that the `value` element in the `list` object should have a
#' class (i.e. `numeric`, `logical`) that is relevant
#' to the parameter that should be updated. For example, if the
#' `parameter` element is equal to `name`, then the `value` element
#' should contain a `character` value.
#' For reference, we provide examples detailing all the various parameters that
#' can be updated below.
#'
#' **Update layer name.**
#' This controls the bold text shown in the header of the layer.
#'
#' \describe{
#' \item{id}{`"LAYERID"`}
#' \item{parameter}{`"name"`}
#' \item{value}{`"SPECIES"`}
#' }
#'
#' **Update layer visibility.**
#' This controls the visible checkbox for a layer.
#' Note that the `value` element must be a `logical` (`TRUE` or `FALSE`) value
#'
#' \describe{
#' \item{id}{`"LAYERID"`}
#' \item{parameter}{`"visible"`}
#' \item{value}{`TRUE`}
#' }
#'
#' **Update the visibility of features within a layer.**
#' This controls the visible checkbox for each feature within a theme.
#' Note that the `value` element must have a value for each feature
#' within the theme (the example below assumes the theme has three features).
#'
#' \describe{
#' \item{id}{`"LAYERID"`}
#' \item{parameter}{`"feature_visible"`}
#' \item{value}{`c(TRUE, FALSE, TRUE)`}
#' }
#'
#' **Update the order of features within a layer.**
#' This controls the relative order of the features within a theme.
#' Note that the `value` element must have a value for each dataset
#' within a layer (e.g. the example below assumes a theme has three features).
#'
#' \describe{
#' \item{id}{`"LAYERID"`}
#' \item{parameter}{`"feature_order"`}
#' \item{value}{`c(TRUE, FALSE, TRUE)`}
#' }
#'
#' @seealso [mapManager()].
#'
#' @export
updateMapManagerLayer <- function(
  session = shiny::getDefaultReactiveDomain(), inputId, value) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(inputId),
    assertthat::noNA(inputId),
    is.list(value))
  assertthat::assert_that(
    assertthat::has_name(value, "parameter"),
    assertthat::is.string(value$parameter),
    assertthat::noNA(value$parameter),
    assertthat::has_name(value, "value"))

  # assert value contains valid settings
  ## define valid parameter names and value classes
  param_names <- c(
    "name", "visible", "feature_order", "feature_visible")
    param_classes <- c(
    "character", "logical", "numeric", "logical")

  ## sanity check
  assertthat::assert_that(
    length(param_names) == length(param_classes),
    msg = "internal validation failed")

  ## coerce integer values to double values
  if (is.integer(value$value)) {
    value$value <- as.double(value$value)
  }

  ## run checks
  assertthat::assert_that(
    value$parameter %in% param_names,
    msg = paste0(
      value$type,
      "s must have a `parameter` equal to one of the following: ",
      paste(paste0("\"", param_names, "\""), collapse = ", "))
  )
  assertthat::assert_that(
    inherits(
      value$value,
      param_classes[[which(param_names == value$parameter)]]),
    msg = paste0(
      "the \"", value$parameter,
      "\" parameter must have a ",
      param_classes[[which(param_names == value$parameter)]],
      " `value`")
  )

  # pass data to widget
  session$sendCustomMessage(
    "mapManager:updateLayer", list(id = inputId, value = value))
}
