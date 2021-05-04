#' Solution settings widget
#'
#' Constructs a widget for maanging the settings for generating solutions.
#'
#' @param x [SolutionSettings] object.
#'
#' @param elementId `character` HTML identifier for the widget.
#'   Defaults to `NULL`.
#'
#' @param width `character` width of the displayed widget.
#'   Defaults to `NULL` such that the widget is automatically sized.
#'
#' @param height `character` width of the displayed widget.
#'   Defaults to `NULL` such that the widget is automatically sized.
#'
#' @section Server value:
#' The widget sends the following values to the server
#' (where `elementId` corresponds to the `elementId`):
#'
#' \describe{
#'
#'   \item{elementId_run}{`character` name to use for a new solution.
#'     This value is updated when the user clicks the button to generate
#'     a new solution.}
#'
#'   \item{elementId_setting}{`list` containing updated settings.
#'     This value is updated when the user updates the settings
#'     (e.g. status, factor, goal) for a [Weight] or [Theme].
#'     Specifically, the `list` contains the following elements:
#'
#'   \describe{
#'     \item{id}{`character` identifier for the theme or weight.}
#'     \item{parameter}{`character` name of the updated parameter.
#'       Available options include: `"status"`, `"factor"`, or `"goal"`.}
#'     \item{value}{new `numeric` or `logical` values.}
#'     \item{type}{`character` indicating if the updated parameter corresponds
#'       to a `theme` or `weight`.}
#'   }
#' }
#' }
#'
#' @examples
#' # TODO.
#'
#' @export
solutionSettings <- function(
  x, width = NULL, height = NULL, elementId = NULL) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "SolutionSettings"))

  # create widget
  htmlwidgets::createWidget(
    name = "solutionSettings",
    x$get_widget_data,
    width = width,
    height = height,
    package = "locationmisc",
    elementId = elementId
  )
}

#' Shiny bindings for solutionSettings
#'
#' Use `solutionSettingsOutput()` to create a user interface element,
#' and `renderSolutionSettings()` to render the widget.
#'
#' @param outputId output variable to read from
#'
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#'
#' @param expr An expression that generates a [solutionSettings()].
#'
#' @param env The environment in which to evaluate \code{expr}.
#'
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name solutionSettings-shiny
#'
#' @export
solutionSettingsOutput <- function(
  outputId, width = "100%", height = "auto"){
  htmlwidgets::shinyWidgetOutput(
    outputId, "solutionSettings", width, height, package = "locationmisc")
}

#' @rdname solutionSettings-shiny
#' @export
renderSolutionSettings <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(
    expr, solutionSettingsOutput, env, quoted = TRUE)
}

# Add custom HTML for the widget (automatically used by htmlwidgets)
solutionSettings_html <- function(id, style, class, ...) {
  htmltools::tags$div( id = id, class = class, style = style)
  # TODO
}
