#' Solution results
#'
#' Constructs a widget for displaying solution results.
#' This widget is designed to be used in conjunction with an existing
#' Leaflet Map within a Shiny web application.
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
  width = NULL, height = NULL, elementId = NULL) {
  # create widget
  htmlwidgets::createWidget(
    name = "solutionResults",
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
          htmltools::tags::div(
            class = "solution-select-container",
            htmltools::tags::label(
              for = paste0(id, "_select"),
              "Solution:"
            ),
            htmltools::tags::select(
              id = paste0(id, "_select"),

            )
          htmltools::tags::label()
<label for="cars">Choose a car:</label>

<select name="cars" id="cars">
  <option value="volvo">Volvo</option>
  <option value="saab">Saab</option>
  <option value="mercedes">Mercedes</option>
  <option value="audi">Audi</option>
</select>

          shiny::selectInput(
             inputId = id,
             label = "Solution:",
             choices = c())
          # container to show results for a given solution
          htmltools::tags$div(
            class = "result"
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
  ### main container
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

  ### sub container
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "multi-theme-single-results-template",
        htmltools::tags$div(
          class = "single-container",
          # TODO
        )
      )
    )

  # return result
  x
}
