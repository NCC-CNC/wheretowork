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
#' The widget sends a `list` with the following values to the server:
#'
#'
#' \describe{
#'
#' \item{id}{`character` identifier for the theme or weight.}
#'
#' \item{setting}{`character` name of the updated setting.
#'   Available options include: `"status"`, `"factor"`, or `"goal"`.}
#'
#' \item{value}{new `numeric` or `logical` values.}
#'
#' \item{type}{`character` indicating if the updated setting corresponds
#'   to a `theme` or `weight`.}
#'
#' }
#'
#' The widget also contains a text box. The server value for this
#' text box is a `character` string. It can be queried using
#' `id_name` where `id` is the argument to `elementId`.
#'
#' The widget also contains a button. The server value for this
#' button is an `integer` indicating the number of times the button
#' has been clicked. It can be queried using `id_button` where
#' `id` is the argument to `elementId`.
#'
#' The widget also contains a color picker. The server value for this
#' button is an `character` indicating the current color. It can be queried
#' using `id_color` where `id` is the argument to `elementId`.
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("solutionSettings")
#' }
#' }
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
    dependencies = c(
        htmltools::htmlDependencies(shiny::icon("map-marked-alt")),
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
            multiple = FALSE,
            open = paste0(id, "_collapseThemePanel"),
            shinyBS::bsCollapsePanel(
              title = htmltools::tags$span(
                shiny::icon("star"),
                "Themes"
              ),
              value = paste0(id, "_collapseThemePanel"),
              htmltools::tags$div(class = "themes")
            ),
            shinyBS::bsCollapsePanel(
              title = htmltools::tags$span(
                shiny::icon("weight-hanging"),
                "Weights"
              ),
              value = paste0(id, "_collapseWeightPanel"),
              htmltools::tags$div(class = "weights")
            ),
            shinyBS::bsCollapsePanel(
              title = htmltools::tags$span(
                shiny::icon("lock"),
                "Includes"
              ),
              value = paste0(id, "_collapseIncludePanel"),
              htmltools::tags$div(class = "includes")
            ),
            shinyBS::bsCollapsePanel(
              title = htmltools::tags$span(
                shiny::icon("cog"),
                "Settings"
              ),
              value = paste0(id, "_collapseParametersPanel"),
              htmltools::tags$div(class = "parameters")
            )
          )
        ),

        ### footer
        htmltools::tags$div(
          class = "solution-footer",
          htmltools::tags$div(
            class = "solution-footer-name",
            shiny::textInput(
              inputId = paste0(id, "_name"),
              NULL,
              value = "",
              width = "100%",
              placeholder = "enter solution name"
            ),
          ),

          htmltools::tags$div(
            class = "solution-footer-color",
            colourpicker::colourInput(
              inputId = paste0(id, "_color"),
              value = "#FF0000",
              showColour = "background",
              label = NULL,
              palette = "limited"
            )
          ),

          htmltools::tags$div(
            class = "solution-footer-button",
            shinyFeedback::loadingButton(
              inputId = paste0(id, "_button"),
              label = "Generate solution",
              loadingLabel = "Optimizing..."
            )
          )
        )
      )
    )

  # add HTML template scaffolds for dynamic content
  ## parameter
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "parameter-setting-template",
        htmltools::tags$div(
          class = paste("parameter-setting solution-setting"),
          ss_header_component_scaffold("parameter"),
          htmltools::tags$div(
            class = "parameter-slider",
            ss_slider_component_scaffold("parameter")
          ),
        )
      )
    )

  ## include
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "include-setting-template",
        htmltools::tags$div(
          class = paste("include-setting solution-setting"),
          ss_header_component_scaffold("include"),
        )
      )
    )

  ## weight
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "weight-setting-template",
        htmltools::tags$div(
          class = paste("weight-setting solution-setting"),
          ss_header_component_scaffold("weight"),
          htmltools::tags$div(
            class = "weight-slider",
            ss_slider_component_scaffold("weight")
          ),
        )
      )
    )

  ## singleTheme
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "single-theme-setting-template",
        htmltools::tags$div(
          class = "single-theme-setting solution-setting",
          ss_header_component_scaffold("theme"),
          ss_goal_component_scaffold("theme")
        )
      )
    )

  ## multiTheme
  ### main container
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "multi-theme-setting-template",
        htmltools::tags$div(
          class = "multi-theme-setting solution-setting",
          ss_header_component_scaffold("theme"),
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
                  ss_group_goal_component_scaffold("theme")
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
          ss_subheader_component_scaffold(),
          ss_goal_component_scaffold("theme")
        )
      )
    )

  # return HTML
  x
}
