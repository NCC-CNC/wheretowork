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
solutionResults <- function(
  x = list(), width = NULL, height = NULL, elementId = NULL) {
  # assert arguments are valid
  assertthat::assert_that(is.list(x))
  if (length(x) > 0) {
    assertthat::assert_that(all_list_elements_inherit(x, "Solution"))
  }

  # prepare parameters
  if (length(x) > 0) {
    p <- list(
      api = list(),
      solutions = lapply(x, function(x) x$get_solution_results_widget_data()))
  } else {
    p <- list(api = list(), solutions = list())
  }

  # create widget
  htmlwidgets::createWidget(
    name = "solutionResults",
    p,
    width = width,
    height = height,
    package = "locationmisc",
    elementId = elementId,
    dependencies = c(
      htmltools::htmlDependencies(shiny::icon("map-marked-alt")))
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
solutionResultsOutput <- function(outputId, width = "100%", height = "auto"){
  htmlwidgets::shinyWidgetOutput(
    outputId, "solutionResults", width, height, package = "locationmisc")
}

#' @rdname solutionResults-shiny
#' @export
renderSolutionResults <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(
    expr, solutionResultsOutput, env, quoted = TRUE)
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
          # select input
          htmltools::tags$div(
            class = "solution-select-container",
            htmltools::tags$label(
              "Solution:"
            ),
            htmltools::tags$select(
              class = "solution-select"
            )
          ),
          # container to show results for a given solution
          htmltools::tags$div(
            class = "solution-result-container"
          )
        )
      )
    )

  # add HTML template scaffolds for dynamic content
  ## statistic
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "statistic-template",
        htmltools::tags$div(
          class  = "solution-result",
          # TODO
        )
      )
    )

  ## weight
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "weight-results-template",
        htmltools::tags$div(
          class  = "solution-result",
          # TODO
        )
      )
    )

  ## singleTheme
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "single-theme-results-template",
        htmltools::tags$div(
          class  = "solution-result",
          # TODO
        )
      )
    )

  ## multiTheme
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "multi-theme-results-template",
        htmltools::tags$div(
          class = "solution-result",
          # TODO
        )
      )
    )

  # return result
  x
}
