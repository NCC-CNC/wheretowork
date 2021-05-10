#' Map manager
#'
#' Constructs a widget for managing items on a Leaflet map.
#' This widget is designed to be used in conjunction with an existing
#' Leaflet Map within a Shiny web application.
#'
#' @param x MapManager object.
#'
#' @inheritParams solutionSettings
#'
#' @section Server value:
#' The widget sends the following values to the server
#' (where `elementId` corresponds to the `elementId`):
#'
#' \describe{
#'
#'   \item{TODO}{`TODO`}
#'
#' }
#'
#' @rdname mapManager-widget
#'
#'
#' @export
mapManager <- function(
  x, width = NULL, height = NULL, elementId = NULL) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "MapManager"))

  # create widget
  htmlwidgets::createWidget(
    name = 'mapManager',
    x$get_map_manager_widget_data(),
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
