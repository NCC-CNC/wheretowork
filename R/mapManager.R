#' Map manager
#'
#' Constructs a widget for managing items on a Leaflet map.
#' This widget is designed to be used in conjunction with an existing
#' Leaflet Map within a Shiny web application.
#'
#' @param mapElementId `character` HTML identifer for Leaflet map.
#'   This should be the argument supplied to the `outputId` parameter in
#'   the [leaflet::leafletOutput()].
#'
#' @param items `list` of objects to display on the map. These objects can
#'  include [Weight], [SingleTheme], [MultiTheme] objects.
#'
#' @inheritParams newSolutionManager
#'
#' @section Server value:
#' The widget sends the following values to the server
#' (where `eId` corresponds to the `elementId`):
#'
#' \describe{
#'
#'   \item{id_run}{`character` name to use for a new solution.
#'     This value is updated when the user clicks the button to generate
#'     a new solution.}
#'
#'   \item{id_setting}{`list` containng updated weight settings.
#'     Specifically, this object contains the following elements:
#'
#'   \describe{
#'     \item{id}{`character` identifier for the theme or weight.}
#'     \item{parameter}{`character` name of the updated parameter.
#'       Available options include: `"status"`, `"factor"`, or `"goal"`.}
#'     \item{value}{`numeric` new setting value.}
#'   }
#' }
#' }
#'
#' @export
mapManager <- function(
  mapElementId, items, width = NULL, height = NULL, elementId = NULL) {
  # TODO
  stop("not implemented")

  # forward options using x
  p = list(
    message = message
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'mapManager',
    p,
    width = width,
    height = height,
    package = 'locationmisc',
    elementId = elementId
  )
}

#' Shiny bindings for mapManager
#'
#' Use `mapManagerOutput()` to create a user interface element,
#' and `renderMapManager()` to render the widget.
#'
#' @param outputId output variable to read from
#'
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#'
#' @param expr An expression that generates a [mapManager()]
#'
#' @param env The environment in which to evaluate \code{expr}.
#'
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name mapManager-shiny
#'
#' @export
mapManagerOutput <- function(outputId, width = "100%", height = "auto"){
  htmlwidgets::shinyWidgetOutput(
    outputId, "mapManager", width, height, package = "locationmisc")
}

#' @rdname mapManager-shiny
#' @export
renderMapManager <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, mapManagerOutput, env, quoted = TRUE)
}

# Add custom HTML for the widget (automatically used by htmlwidgets)
mapManager_html <- function(id, style, class, ...) {
  htmltools::tags$div( id = id, class = class, style = style)
  # TODO
}
