#' @include internal.R
NULL

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
#' The widget also contains two buttons. The server values for these
#' buttons is an `integer` indicating the number of times they
#' has been clicked. They can be queried using `id_start_button`
#' and `id_stop_button` where `id` is the argument to `elementId`.
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
solutionSettings <- function(x, width = NULL, height = NULL, elementId = NULL) {
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
    package = "wheretowork",
    elementId = elementId,
    dependencies = c(
      htmltools::htmlDependencies(shiny::icon("map-marked-alt")),
      htmltools::htmlDependencies(
        shinyBS::bsCollapse(shinyBS::bsCollapsePanel("id"))
      )
    )
  )
}

#' Shiny bindings for `solutionSettings`
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
solutionSettingsOutput <- function(outputId, width = "100%", height = "auto") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "solutionSettings", width, height,
    package = "wheretowork"
  )
}

#' @rdname solutionSettings-shiny
#' @export
renderSolutionSettings <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(
    expr, solutionSettingsOutput, env,
    quoted = TRUE
  )
}

# Add custom HTML for the widget (automatically used by htmlwidgets)
solutionSettings_html <- function(id, style, class, ...) {
  # HTML scaffold
  x <-
    htmltools::tags$div(
      id = id, class = class, style = style,
      htmltools::div(
        class = "solution-settings",
        shinyBS::bsCollapse(
          id = paste0(id, "_collapse"),
          multiple = FALSE,
          open = paste0(id, "_collapseThemePanel"),
          shinyBS::bsCollapsePanel(
            title = htmltools::tags$span(
              shinyBS::tipify(
                el = htmltools::tags$span(
                  shiny::icon("star"),
                  "Themes"
                ),
                title = paste(
                  "Themes describe facets of biodiversity that are important",
                  "for conservation (e.g. species, habitats, ecosystems).",
                  "To help safeguard them,",
                  "you can set goals for themes to increase their coverage",
                  "within solutions."
                ),
                options = list(container = "body")
              )
            ),
            value = paste0(id, "_collapseThemePanel"),
            htmltools::tags$div(
              class = "panel-content-inner",
              htmltools::tags$div(class = "themes")
            )
          ),
          shinyBS::bsCollapsePanel(
            title = htmltools::tags$span(
              shinyBS::tipify(
                el = htmltools::tags$span(
                  shiny::icon("weight-hanging"),
                  "Weights"
                ),
                title = paste(
                  "Weights describe properties of places that",
                  "should be considered when generating solutions.",
                  "To alter how much they are covered by solutions,",
                  "you can set factors for them.",
                  "Positive factors (i.e. values > 0) indicate that",
                  "it is more important for solutions to cover a weight",
                  "(e.g. landscape naturalness).",
                  "Negative factors (i.e. values < 0) indicate that",
                  "it is more important for solutions to avoid a weight",
                  "(e.g. human population density)."
                ),
                options = list(container = "body")
              )
            ),
            value = paste0(id, "_collapseWeightPanel"),
            htmltools::tags$div(
              class = "panel-content-inner",
              htmltools::tags$div(class = "weights")
            )
          ),
          shinyBS::bsCollapsePanel(
            title = htmltools::tags$span(
              shinyBS::tipify(
                el = htmltools::tags$span(
                  shiny::icon("lock"),
                  "Includes"
                ),
                title = paste(
                  "Includes describe places that are currently managed for",
                  "conservation",
                  "(e.g. protected areas, national parks).",
                  "To build on existing conservation efforts,",
                  "you can toggle includes on so that they",
                  "are selected within solutions."
                ),
                options = list(container = "body")
              )
            ),
            value = paste0(id, "_collapseIncludePanel"),
            htmltools::tags$div(
              class = "panel-content-inner",
              htmltools::tags$div(class = "includes")
            )
          ),
          shinyBS::bsCollapsePanel(
            title = htmltools::tags$span(
              shinyBS::tipify(
                el = htmltools::tags$span(
                  shiny::icon("cog"),
                  "Settings"
                ),
                title = paste(
                  "Settings control the behavior of the optimization process."
                ),
                options = list(container = "body")
              )
            ),
            value = paste0(id, "_collapseParametersPanel"),
            htmltools::tags$div(
              class = "panel-content-inner",
              htmltools::tags$div(class = "parameters")
            )
          )
        )
      ),

      ### footer
      htmltools::tags$div(
        class = "solution-footer",
        htmltools::tags$div(
          class = "solution-footer-name",
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          `data-container` = "body",
          title = "Specify a name for the new solution",
          shiny::textInput(
            inputId = paste0(id, "_name"),
            NULL,
            value = "",
            width = "100%",
            placeholder = "enter name (required)"
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
          class = "solution-footer-start-button",
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          `data-container` = "body",
          title = paste(
            "Generate a new solution using the Themes, Weights, Includes,",
            "and Settings"
          ),
          shinyFeedback::loadingButton(
            inputId = paste0(id, "_start_button"),
            label = "Optimize!",
            class = "btn btn-primary",
            loadingLabel = "",
            style = "width: 86px;"
          )
        ),
        htmltools::tags$div(
          class = "solution-footer-stop-button",
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          `data-container` = "body",
          title = "Stop optimizing",
          shinyBS::bsButton(
            inputId = paste0(id, "_stop_button"),
            label = "",
            icon = shiny::icon("ban"),
            style = "danger"
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
            `data-toggle` = "tooltip",
            `data-placement` = "bottom",
            `data-container` = "body",
            `data-trigger` = "hover",
            title = "Set the parameter value",
            ss_slider_component_scaffold()
          )
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
            `data-toggle` = "tooltip",
            `data-placement` = "bottom",
            `data-container` = "body",
            `data-trigger` = "hover",
            title = "Set the factor",
            ss_slider_component_scaffold()
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
          ss_goal_component_scaffold()
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
                  ss_group_goal_component_scaffold()
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
          ss_goal_component_scaffold()
        )
      )
    )

  # add HTML template scaffolds for static content
  ## no weights scaffold
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "no-weights-template",
        htmltools::tags$div(
          class = paste("empty-setting solution-setting"),
          htmltools::tags$label(
            "No weights specified."
          )
        )
      )
    )

  ## no includes scaffold
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "no-includes-template",
        htmltools::tags$div(
          class = paste("empty-setting solution-setting"),
          htmltools::tags$label(
            "No includes specified."
          )
        )
      )
    )

  # return HTML
  x
}
