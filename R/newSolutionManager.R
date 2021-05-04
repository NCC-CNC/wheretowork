#' New solution manager
#'
#' Constructs a widget for generating new solutions and managing their
#' associated settings.
#'
#' @param themes `list` of `Theme` objects to include in the manager.
#'
#' @param weights `list` of `Weight` objects to include in the manager.
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
newSolutionManager <- function(
  themes, weights, width = NULL, height = NULL, elementId = NULL) {
  # assert arguments are valid
  TODO

  # forward options using x
  p = list(
    message = message
  )

  # create widget
  htmlwidgets::createWidget(
    name = "newSolutionManager",
    p,
    width = width,
    height = height,
    package = "locationmisc",
    elementId = elementId
  )
}

#' Shiny bindings for newSolutionManager
#'
#' Use `newSolutionManagerOutput()` to create a user interface element,
#' and `renderNewSolutionManager()` to render the widget.
#'
#' @param outputId output variable to read from
#'
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#'
#' @param expr An expression that generates a [newSolutionManager()].
#'
#' @param env The environment in which to evaluate \code{expr}.
#'
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name newSolutionManager-shiny
#'
#' @export
newSolutionManagerOutput <- function(
  outputId, width = "100%", height = "auto"){
  htmlwidgets::shinyWidgetOutput(
    outputId, "newSolutionManager", width, height, package = "locationmisc")
}

#' @rdname newSolutionManager-shiny
#' @export
renderNewSolutionManager <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(
    expr, newSolutionManagerOutput, env, quoted = TRUE)
}

# Add custom HTML for the widget (automatically used by htmlwidgets)
newSolutionManager_html <- function(id, style, class, ...) {
  htmltools::tags$div( id = id, class = class, style = style)
  # TODO
}
