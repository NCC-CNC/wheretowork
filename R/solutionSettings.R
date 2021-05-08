#' Solution settings widget
#'
#' Constructs a widget for managing the settings for generating solutions.
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
#' \item{id}{`character` identifier for the theme or weight.}
#'
#' \item{parameter}{`character` name of the updated parameter.
#'   Available options include: `"status"`, `"factor"`, or `"goal"`.}
#'
#' \item{value}{new `numeric` or `logical` values.}
#'
#' \item{type}{`character` indicating if the updated parameter corresponds
#'   to a `theme` or `weight`.}
#'
#' }
#'
#' @examples
#' # TODO.
#'
#' @rdname solutionSettings-widget
#'
#' @export
solutionSettings <- function(
  x, width = NULL, height = NULL, elementId = NULL) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "SolutionSettings"))

  # prepare data
  p <- x$get_widget_data()
  p$api <- list() # enable API

  # create widget
  htmlwidgets::createWidget(
    name = "solutionSettings",
    p,
    width = width,
    height = height,
    package = "locationmisc",
    elementId = elementId,
    dependencies =
      c(
        htmltools::htmlDependencies(shiny::icon("map-marked-alt")),
        htmltools::htmlDependencies(
          shinyBS::bsCollapse(shinyBS::bsCollapsePanel("id"))),
        htmltools::htmlDependencies(
          shinyBS::bsCollapse(shinyBS::bsCollapsePanel("id"))))
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
  # HTML scaffold
  x <-
    htmltools::tags$div(
      id = id, class = class, style = style,
      htmltools::div(
        class = "solution-settings-container",
        htmltools::div(
          class = "solution-settings",
          shinyBS::bsCollapse(
            id = paste0(id, "_collapse"),
            multiple = TRUE,
            open = paste0(id, c("_collapseThemePanel", "_collapseWeightPanel")),
            shinyBS::bsCollapsePanel(
              title = "Themes",
              value = paste0(id, "_collapseThemePanel"),
              htmltools::tags$div(class = "themes")
            ),
            shinyBS::bsCollapsePanel(
              title = "Weights",
              value = paste0(id, "_collapseWeightPanel"),
              htmltools::tags$div(class = "weights")
            )
          )
        )
      )
    )


  # add HTML template scaffolds for dynamic content
  ## weightFactor
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "weight-setting-template",
        htmltools::tags$div(
          class = paste("weight-setting solution-setting"),
          header_component_scaffold(),
          htmltools::tags$div(
            class = "weight-slider",
            slider_component_scaffold()
          )
        )
      )
    )

  ## singleThemeGoal
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "single-theme-setting-template",
        htmltools::tags$div(
          class = "single-theme-setting solution-setting",
          icon_component_scaffold(),
          header_component_scaffold(),
          goal_component_scaffold()
        )
      )
    )

  ## multiThemeGoal
  ### main container
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "multi-theme-setting-template",
        htmltools::tags$div(
          class = "multi-theme-setting solution-setting",
          icon_component_scaffold(),
          header_component_scaffold(),
          htmltools::tags$div(
            class = "main",
            shiny::tabsetPanel(
              type = "tabs",
              id = "view",
              ## group view panel
              shiny::tabPanel(
                "group",
                htmltools::tags$div(
                  class = "group-view",
                  group_goal_component_scaffold()
                )
              ),
              ## single view
              shiny::tabPanel(
                "single",
                htmltools::tags$div(
                  class = "single-view"
                )
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
        class = "multi-theme-single-setting-template",
        htmltools::tags$div(
          class = "single-container",
          subicon_component_scaffold(),
          subheader_component_scaffold(),
          goal_component_scaffold()
        )
      )
    )

  # return HTML
  x
}
