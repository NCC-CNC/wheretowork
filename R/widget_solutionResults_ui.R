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

#' Shiny bindings for `solutionResults`
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
              selected = NULL,
              options = list(
                `dropdown-align-right` = "true",
                `container` = "body"
              )
            ),
            # modal button
            htmltools::tags$div(
              class = "solution-button-container",
              `data-toggle` = "tooltip",
              `data-placement` = "top",
              `data-container` = "body",
              `data-placement` = "bottom",
              title = "View solution results in tables",
              # button to import data
              shinyBS::bsButton(
                inputId = paste0(id, "_button"),
                label = "",
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
                  shinyBS::tipify(
                    el = htmltools::tags$span(
                      shiny::icon("chart-line"),
                      "Summary"
                    ),
                    title = paste(
                      "Summary of the solution. This panel shows the Settings",
                      "used to generate the solution, and statistics",
                      "that describe its spatial configuration."
                    ),
                    options = list(container = "body")
                  )
                ),
                value = paste0(id, "_collapseStatisticPanel"),
                htmltools::tags$div(
                  class = "panel-content-inner",
                  htmltools::tags$h4("Settings"),
                  htmltools::tags$div(class = "parameters"),
                  htmltools::tags$h4("Statistics"),
                  htmltools::tags$div(class = "statistics")
                )
              ),
              shinyBS::bsCollapsePanel(
                title = htmltools::tags$span(
                  shinyBS::tipify(
                    el = htmltools::tags$span(
                      shiny::icon("star"),
                      "Themes"
                    ),
                    title = paste(
                      "Theme results for the solution.",
                      "This panel shows how well the Themes are covered",
                      "by the solution. It also shows how well the Themes are",
                      "covered by the Includes used to generate the solution,",
                      "and Theme goals used to generate the solution."
                    ),
                    options = list(container = "body")
                  )
                ),
                value = paste0(id, "_collapseThemePanel"),
                htmltools::tags$div(
                  htmltools::tags$div(
                    class = "panel-content-inner",
                    htmltools::tags$div(class = "themes")
                  ),
                  htmltools::tags$div(
                    class = "legend",
                    htmltools::tags$span(
                      class = "legend-item",
                      htmltools::tags$span(class = "legend-current-symbol"),
                      htmltools::tags$label(
                        class = "legend-current-label",
                        `data-toggle` = "tooltip",
                        `data-placement` = "top",
                        `data-container` = "body",
                        title = paste(
                          "Coverage by Includes used to generate solution"
                        ),
                        "Includes"
                      ),
                    ),
                    htmltools::tags$span(
                      class = "legend-item",
                      htmltools::tags$span(class = "legend-goal-symbol"),
                      htmltools::tags$label(
                        class = "legend-goal-label",
                        `data-toggle` = "tooltip",
                        `data-placement` = "top",
                        `data-container` = "body",
                        title = "Goal used to generate solution",
                        "Goal"
                      ),
                    ),
                    htmltools::tags$span(
                      class = "legend-item",
                      htmltools::tags$span(class = "legend-solution-symbol"),
                      htmltools::tags$label(
                        class = "legend-solution-label",
                        `data-toggle` = "tooltip",
                        `data-placement` = "top",
                        `data-container` = "body",
                        title = "Coverage by solution",
                        "Solution"
                      )
                    )
                  )
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
                      "Weight results for the solution.",
                      "This panel shows how well the Weights are covered",
                      "by the solution. It also shows how well the Weights are",
                      "covered by the Includes used to generate the solution,",
                      "and Weight factors used to generate the solution."
                    ),
                    options = list(container = "body")
                  )
                ),
                value = paste0(id, "_collapseWeightPanel"),
                htmltools::tags$div(
                  htmltools::tags$div(
                    class = "panel-content-inner",
                    htmltools::tags$div(class = "weights")
                  ),
                  htmltools::tags$div(
                    class = "legend",
                    htmltools::tags$span(
                      class = "legend-item",
                      htmltools::tags$span(class = "legend-current-symbol"),
                      htmltools::tags$label(
                        class = "legend-current-label",
                        `data-toggle` = "tooltip",
                        `data-placement` = "top",
                        `data-container` = "body",
                        title = paste(
                          "Coverage by Includes used to generate solution"
                        ),
                        "Includes"
                      ),
                    ),
                    htmltools::tags$span(
                      class = "legend-item",
                      htmltools::tags$span(class = "legend-solution-symbol"),
                      htmltools::tags$label(
                        class = "legend-solution-label",
                        `data-toggle` = "tooltip",
                        `data-placement` = "top",
                        `data-container` = "body",
                        title = "Coverage by solution",
                        "Solution"
                      )
                    )
                  )
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
                      "Includes results for the solution.",
                      "This panel shows how well the Includes are covered",
                      "by the solution."
                    ),
                    options = list(container = "body")
                  )
                ),
                value = paste0(id, "_collapseIncludePanel"),
                htmltools::tags$div(
                  htmltools::tags$div(
                    class = "panel-content-inner",
                    htmltools::tags$div(class = "includes")
                  ),
                  htmltools::tags$div(
                    class = "legend",
                    htmltools::tags$span(
                      class = "legend-item",
                      htmltools::tags$span(class = "legend-solution-symbol"),
                      htmltools::tags$label(
                        class = "legend-solution-label",
                        `data-toggle` = "tooltip",
                        `data-placement` = "top",
                        `data-container` = "body",
                        title = "Coverage by solution",
                        "Solution"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

  # add HTML template scaffolds for static content
  ## no weights specified
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "no-weights-template",
        htmltools::tags$div(
          class = paste("empty-result"),
          htmltools::tags$label(
            "No weights specified."
          )
        )
      )
    )
  ## no includes specified
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "no-includes-template",
        htmltools::tags$div(
          class = paste("empty-result"),
          htmltools::tags$label(
            "No includes specified."
          )
        )
      )
    )

  # return result
  x
}
