#' @include internal.R
NULL

#' Import settings
#'
#' Constructs a widget for specifying settings for importing data.
#' This widget is designed to be used within a Shiny web application.
#'
#' @param buttonId `character` containing the identifier for the button HTML
#'  element to accompany this widget.
#'
#' @param x `character` vector containing the field/layer names for the
#'   dataset. A `NULL` value may also be specified to initialize the widget
#'   without any field/layer names.
#'
#' @inheritParams solutionSettings
#'
#' @section Server value:
#' A `list` object containing a `list` element for each field/layer name
#' containing:
#' \describe{
#' \item{name}{`character` name of the field/layer.}
#' \item{import}{`logical` value indicating if the field/layer should be used.}
#' \item{type}{`character` value. Available options are
#'  `"Theme"`, `"Weight"`, or `"Include"`.}
#' \item{nonce}{`numeric` random value used to ensure that updates trigger
#'  when the button is clicked.}
#' }
#'
#' @rdname importSettings-widget
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("importSettings")
#' }
#' }
#'
#' @export
importSettings <- function(buttonId, x = NULL, width = NULL, height = NULL,
                           elementId = NULL) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(buttonId),
    assertthat::noNA(buttonId)
  )
  if (!is.null(x)) {
    assertthat::assert_that(
      is.character(x),
      length(x) > 0,
      assertthat::noNA(x)
    )
  }

  # prepare parameters
  p <- list(api = list(), buttonId = buttonId, value = x)

  # create widget
  htmlwidgets::createWidget(
    name = "importSettings",
    p,
    width = width,
    height = height,
    package = "wheretowork",
    elementId = elementId
  )
}

#' Shiny bindings for `importSettings`
#'
#' Use `importSettingsOutput()` to create a user interface element,
#' and `renderImportSettings()` to render the widget.
#'
#' @param outputId output variable to read from
#'
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#'
#' @param expr An expression that generates a [importSettings()]
#'
#' @param env The environment in which to evaluate \code{expr}.
#'
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name importSettings-shiny
#'
#' @export
importSettingsOutput <- function(outputId, width = "100%", height = "auto") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "importSettings", width, height,
    package = "wheretowork"
  )
}

#' @rdname importSettings-shiny
#' @export
renderImportSettings <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(
    expr, importSettingsOutput, env,
    quoted = TRUE
  )
}

# Add custom HTML for the widget (automatically used by htmlwidgets)
importSettings_html <- function(id, style, class, ...) {
  # HTML scaffold
  x <-
    htmltools::tags$div(
      id = id, class = class, style = style,
      htmltools::div(
        class = "import-settings-container",
        htmltools::div(
          class = "import-settings",
          # layer container
          htmltools::tags$div(
            class = "layers",
          )
        )
      )
    )

  # add HTML template scaffolds for dynamic content
  ## layer
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "layer-settings-template",
        htmltools::div(
          class = "layer-settings input-group",
          htmltools::span(
            class = "input-group-addon",
            htmltools::tags$input(
              class = "checkbox checkbox-inline",
              type = "checkbox"
            )
          ),
          htmltools::tags$p(class = "form-control"),
          htmltools::tags$select(
            class = "form-control",
            htmltools::tags$option("Theme"),
            htmltools::tags$option("Weight"),
            htmltools::tags$option("Include"),
            htmltools::tags$option("Exclude"),
          )
        )
      )
    )

  # return result
  x
}
