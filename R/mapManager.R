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
#' The widget sends a `list` with the following values to the server:
#'
#' \describe{
#'
#'   \item{parameter}{`character` name of the updated parameter.
#'     Available options are: `"visible"` and `"order"`.}
#'
#'   \item{value}{`numeric` or `logical` values.}
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
    name = "mapManager",
    x$get_widget_data(),
    width = width,
    height = height,
    package = "locationmisc",
    elementId = elementId,
    dependencies = c(
      htmltools::htmlDependencies(shiny::icon("map-marked-alt")))
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
  # HTML scaffold
  x <-
    htmltools::tags$div(
      id = id, class = class, style = style,
      htmltools::div(
        class = "map-manager-container",
        htmltools::div(
          class = "map-manager",
          htmltools::tags$div(
            class = "layers"
          )
        )
      )
    )

  # add HTML template scaffolds for dynamic content
  ## weight
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "weight-layer-template",
        htmltools::tags$div(
          class  = "map-manager-layer",
          htmltools::tags$div(
            class = "overlay",
              htmltools::tags$div(
                class = "weight-layer",
                mm_header_component_scaffold(),
                htmltools::tags$div(
                  class = "layer-legend-container",
                  mm_legend_component_scaffold()
                )
              )
          )
        )
      )
    )

  ## singleTheme
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "single-theme-layer-template",
        htmltools::tags$div(
          class  = "map-manager-layer",
          htmltools::tags$div(
            class = "overlay",
              htmltools::tags$div(
                class = "theme-layer",
                mm_header_component_scaffold(),
                htmltools::tags$div(
                  class = "layer-legend-container",
                  mm_legend_component_scaffold()
                )
              )
          )
        )
      )
    )

  ## multiTheme
  ### main container
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "multi-theme-layer-template",
        htmltools::tags$div(
          class = "map-manager-layer",
          htmltools::tags$div(
            class = "overlay",
              htmltools::tags$div(
                class = "theme-layer",
                mm_header_component_scaffold(),
                htmltools::tags$div(
                  class = "main"
                )
              )
          )
        )
      )
    )

  ### sub container
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "multi-theme-single-layer-template",
        htmltools::tags$div(
          class = "single-container",
          htmltools::tags$div(
            htmltools::tags$div(
              class = "overlay",
                htmltools::tags$div(
                  class = "sub-layer",
                  mm_subheader_component_scaffold(),
                  htmltools::tags$div(
                    class = "layer-legend-container",
                    mm_legend_component_scaffold()
                  )
                )
            )
          )
        )
      )
    )

  # return result
  x
}
