#' @include internal.R
NULL

#' Export sidebar pane
#'
#' Constructs a sidebar pane for exporting data.
#'
#' @inheritParams solutionResultsSidebarPane
#'
#' @inherit importModal examples
#'
#' @inherit solutionResultsSidebarPane details return
#'
#' @export
exportSidebarPane <- function(id) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(id),
    assertthat::noNA(id))

  # create sidebar
  leaflet.extras2::sidebar_pane(
    title = "Save data",
    id = id,
    icon = NULL,

    # sidebar pane content
    htmltools::tags$div(
      class = "sidebar-pane-content",

      ## help text
      shiny::p("Select data to save to your computer."),

      ## select columns
      shiny::selectizeInput(
        inputId = paste0(id, "_fields"),
        label = "Select data",
        choices = c(),
        multiple = TRUE,
        width = "100%"
      ),

      ## upload button
      shiny::downloadButton(
        outputId = paste0(id, "_button"),
        label = "Save",
        icon = shiny::icon("download")
      )
    )
  )
}
