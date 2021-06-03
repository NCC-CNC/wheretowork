#' @include internal.R SolutionSettings-class.R solutionSettings.R
NULL

#' Update solution settings widget
#'
#' Change the settings for solution settings widget on the client.
#' Specifically, change the parameters for a theme or weight depicted
#' in a solution settings widget.
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
#' the information necessary to update a theme or weight.
#' Broadly speaking, it should contain the following elements:
#'
#' \describe{
#' \item{id}{`character` value with the identifier for the theme or weight.}
#' \item{parameter}{`character` value with the name of the parameter to update.}
#' \item{value}{`ANY` new value for the parameter.}
#' \item{type}{`character` value indicating if the setting is a `theme`
#'   or `weight`.}
#' }
#'
#' Note that the `value` element in the `list` object should have a
#' class (i.e. `numeric`, `logical, or `character`) that is relevant
#' to the parameter that should be updated. For example, if the
#' `parameter` element is equal to `name`, then the `value` element
#' should contain a `character` value.
#' For reference, we provide examples detailing all the various parameters that
#' can be updated below.
#'
#' ## Themes
#'
#' Here we detail all possible parameters that can be updated for
#' themes.
#'
#' **Update the name.**
#' This controls the bold text shown in the header of the theme.
#'
#' \describe{
#' \item{id}{`"THEMEID"`}
#' \item{parameter}{`"name"`}
#' \item{value}{`"SPECIES"`}
#' \item{type}{`"theme"`}
#' }
#'
#' **Update the status.**
#' This controls the large switch shown in the header of theme.
#' Note that the `value` element must be a `logical` (`TRUE` or `FALSE`) value.
#'
#' \describe{
#' \item{id}{`"THEMEID"`}
#' \item{parameter}{`"status"`}
#' \item{value}{`TRUE`}
#' \item{type}{`"theme"`}
#' }
#'
#' **Update the view.**
#' This controls whether the "group" or "single" tab is active for themes with
#' multiple features.
#' It does not have any effect for themes with a single feature.
#' Note that the `value` element must be a `character` value equal to
#' `"group"` or `"single"`.
#'
#' \describe{
#' \item{id}{`"THEMEID"`}
#' \item{parameter}{`"view"`}
#' \item{value}{`"group"`}
#' \item{type}{`"theme"`}
#' }
#'
#' **Update the group goal.**
#' This controls the slider present under the "group" tab.
#' It does not have any effect for themes with a single features.
#'
#' \describe{
#' \item{id}{`"THEMEID"`}
#' \item{parameter}{`"group_goal"`}
#' \item{value}{`0.7`}
#' \item{type}{`"theme"`}
#' }
#'
#' **Update the feature status.**
#' This controls the switches present under the "single" tab.
#' To ensure compatibility between themes with a single feature
#' and themes with multiple features, this will update the status
#' for a theme with a single feature.
#' Note that the `value` element must have a value for each feature
#' within the theme (the example below assumes the theme has three features).
#'
#' \describe{
#' \item{id}{`"THEMEID"`}
#' \item{parameter}{`"feature_status"`}
#' \item{value}{`c(TRUE, FALSE, TRUE)`}
#' \item{type}{`"theme"`}
#' }
#'
#' **Update the feature goals.**
#' This controls the sliders present under the "single" tab.
#' To ensure compatibility between themes with a single feature
#' and themes with multiple features, this will update the status
#' for a theme with a single feature.
#' Note that the `value` element must have a value for each feature
#' within the theme (the example below assumes the theme has three features).
#'
#' \describe{
#' \item{id}{`"THEMEID"`}
#' \item{parameter}{`"feature_goal"`}
#' \item{value}{`c(0.3, 0.1, 0.7)`}
#' \item{type}{`"theme"`}
#' }
#'
#'
#' ## Weights
#'
#' Here we detail all possible parameters that can be updated for
#' weights.
#'
#' **Update the name.**
#' This controls the bold text shown in the header of the weight.
#'
#' \describe{
#' \item{id}{`"WEIGHTID"`}
#' \item{parameter}{`"name"`}
#' \item{value}{`"SPECIES"`}
#' \item{type}{`"weight"`}
#' }
#'
#' **Update the status.**
#' This controls the large switch shown in the header of weight.
#' Note that the `value` element must be a `logical` (`TRUE` or `FALSE`) value.
#'
#' \describe{
#' \item{id}{`"WEIGHTID"`}
#' \item{parameter}{`"status"`}
#' \item{value}{`TRUE`}
#' \item{type}{`"weight"`}
#' }
#'
#' **Update the factor.**
#' This controls the slider shown for the weight.
#'
#' \describe{
#' \item{id}{`"WEIGHTID"`}
#' \item{parameter}{`"factor"`}
#' \item{value}{`0.1`}
#' \item{type}{`"weight"`}
#' }
#'
#' @seealso [solutionSettings()].
#'
#' @export
updateSolutionSettings <- function(
  session = shiny::getDefaultReactiveDomain(), inputId, value) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(inputId),
    assertthat::noNA(inputId),
    is.list(value))
  assertthat::assert_that(
    assertthat::has_name(value, "id"),
    assertthat::is.string(value$id),
    assertthat::noNA(value$id),
    assertthat::has_name(value, "parameter"),
    assertthat::is.string(value$parameter),
    assertthat::noNA(value$parameter),
    assertthat::has_name(value, "value"),
    assertthat::has_name(value, "type"),
    assertthat::is.string(value$type),
    assertthat::noNA(value$type))

  # assert value contains valid settings
  ## define valid parameter names and value classes
  assertthat::assert_that(value$type %in% c("weight", "theme"))
  if (identical(value$type, "theme")) {
    param_names <- c(
      "name", "status", "group_goal", "feature_goal", "feature_status", "view")
    param_classes <- c(
      "character", "logical", "numeric", "numeric", "logical", "character")
  } else if (identical(value$type, "weight")) {
    param_names <- c("name", "status", "factor")
    param_classes <- c("character", "logical", "numeric")
  }

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
  if (identical(value$parameter, "view")) {
    assertthat::assert_that(
      value$value %in% c("single", "group"),
      msg = paste0(
        "the \"view\" parameter must have a \"single\" ",
        "or \"group\" `value`."))
  }

  # pass data to widget
  session$sendCustomMessage(
    "solutionSettings:update", list(id = inputId, value = value))
}
