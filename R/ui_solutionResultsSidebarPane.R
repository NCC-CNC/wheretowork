#' @include internal.R widget_solutionResults_ui.R
NULL

#' Solution results sidebar pane
#'
#' Constructs a sidebar pane for displaying solution results.
#'
#' @param id `character` identifier for the sidebar pane.
#'
#' @param solutionResultsId `character` identifier for the
#'   [solutionResults()] widget to create within the sidebar pane.
#'   This widget is used to  display results for solutions.
#'   Defaults to `paste0(id, "_results")`.
#'
#' @details
#' This is designed to be used as an argument to
#' [leaflet.extras2::sidebar_tabs] when specifying
#' the user interface for a Shiny web application.
#'
#' @return A `shiny.tag` object with the sidebar pane.
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("solutionResultsSidebarPane")
#' }
#' }
#'
#' @export
solutionResultsSidebarPane <- function(
  id,
  solutionResultsId = paste0(id, "_results")) {
  # assert arguments are valid
  assertthat::assert_that(
    ### id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ### solutionResultsId
    assertthat::is.string(solutionResultsId),
    assertthat::noNA(solutionResultsId))

  # create sidebar widget
  ## create sidebar
  w <-
    leaflet.extras2::sidebar_pane(
      title = "Solution results",
      id = id,
      icon = NULL,
      htmltools::tags$div(
        class = "sidebar-pane-content",
        htmltools::tags$div(
          class = "solution-results-pane",
          htmltools::tags$div(
            class = "widget-container",
            solutionResultsOutput(solutionResultsId, height = "100%")
          )
        )
      )
    )

  # return result
  w

}
