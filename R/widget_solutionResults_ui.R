#' @include internal.R
NULL

#' Solution results
#'
#' Constructs a widget for displaying solution results.
#' This widget is designed to be used in conjunction with an existing
#' Leaflet Map within a Shiny web application.
#'
#' @param x `list` containing [Solution] objects.
#'   Defaults to an empty list object.
#'
#' @inheritParams solutionSettings
#'
#' @section Server value:
#' The widget does not send any server values.
#'
#' @rdname solutionResults-widget
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("solutionResults")
#' }
#' }
#'
#' @export
solutionResults <- function(x = list(), width = NULL, height = NULL,
                            elementId = NULL) {
  # assert arguments are valid
  assertthat::assert_that(is.list(x))
  if (length(x) > 0) {
    assertthat::assert_that(all_list_elements_inherit(x, "Solution"))
  }

  # prepare parameters
  if (length(x) > 0) {
    p <- list(
      api = list(),
      solutions = lapply(x, function(x) x$get_solution_results_widget_data())
    )
  } else {
    p <- list(api = list(), solutions = list())
  }

  # create widget
  htmlwidgets::createWidget(
    name = "solutionResults",
    p,
    width = width,
    height = height,
    package = "wheretowork",
    elementId = elementId,
    dependencies = c(
      htmltools::htmlDependencies(shiny::icon("map-marked-alt")),
      htmltools::htmlDependencies(shinyBS::bsCollapsePanel("id")),
      htmltools::htmlDependencies(shinyWidgets::pickerInput("id", "x", "y"))
    )
  )
}

#' Shiny bindings for solutionResults
#'
#' Use `solutionResultsOutput()` to create a user interface element,
#' and `renderSolutionResults()` to render the widget.
#'
#' @param outputId output variable to read from
#'
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#'
#' @param expr An expression that generates a [solutionResults()]
#'
#' @param env The environment in which to evaluate \code{expr}.
#'
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name solutionResults-shiny
#'
#' @export
solutionResultsOutput <- function(outputId, width = "100%", height = "auto") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "solutionResults", width, height,
    package = "wheretowork"
  )
}

#' @rdname solutionResults-shiny
#' @export
renderSolutionResults <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(
    expr, solutionResultsOutput, env,
    quoted = TRUE
  )
}

# Add custom HTML for the widget (automatically used by htmlwidgets)
solutionResults_html <- function(id, style, class, ...) {
  # HTML scaffold
  x <-
    htmltools::tags$div(
      id = id, class = class, style = style,
      htmltools::div(
        class = "solution-results-container",
        htmltools::div(
          class = "solution-results",
          # header
          htmltools::tags$div(
            class = "solution-results-header",
            # select input
            horizontalPickerInput(
              inputId = paste0(id, "_select"),
              label = "Solution:",
              choices = c("NA" = "NA"),
              selected = NULL
            ),
            # modal button
            htmltools::tags$div(
              class = "solution-button-container",
              # button to import data
              shinyBS::bsButton(
                inputId = paste0(id, "_button"),
                label = "View data",
                icon = shiny::icon("table"),
                style = "primary",
                type = "action"
              )
            )
          ),
          # modal
          solutionResultsModal(
            id = paste0(id, "_modal"),
            trigger = paste0(id, "_button")
          ),
          # accordion panels
          htmltools::tags$div(
            class = "solution-results-main",
            shinyBS::bsCollapse(
              id = paste0(id, "_collapse"),
              multiple = FALSE,
              open = paste0(id, "_collapseStatisticPanel"),
              shinyBS::bsCollapsePanel(
                title = htmltools::tags$span(
                  shiny::icon("chart-line"),
                  "Summary"
                ),
                value = paste0(id, "_collapseStatisticPanel"),
                htmltools::tags$div(
                  class = "panel-content-inner",
                  htmltools::tags$h4("Parameters"),
                  htmltools::tags$div(class = "parameters"),
                  htmltools::tags$h4("Statistics"),
                  htmltools::tags$div(class = "statistics")
                )
              ),
              shinyBS::bsCollapsePanel(
                title = htmltools::tags$span(
                  shiny::icon("star"),
                  "Themes"
                ),
                value = paste0(id, "_collapseThemePanel"),
                htmltools::tags$div(
                  class = "panel-content-inner",
                  htmltools::tags$div(class = "themes")
                )
              ),
              shinyBS::bsCollapsePanel(
                title = htmltools::tags$span(
                  shiny::icon("weight-hanging"),
                  "Weights"
                ),
                value = paste0(id, "_collapseWeightPanel"),
                htmltools::tags$div(
                  class = "panel-content-inner",
                  htmltools::tags$div(class = "weights")
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
