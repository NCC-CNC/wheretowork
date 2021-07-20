#' @include internal.R widget_solutionSettings_ui.R
NULL

#' New solution sidebar pane
#'
#' Constructs a sidebar pane for generating new solutions.
#'
#' @param solutionSettingsId `character` identifier for the
#'   [solutionSettings()] widget to create within the sidebar pane.
#'   This widget is used to control the settings for new solutions.
#'   Defaults to `NULL` such that the default argument is
#'   `paste0(id, "_settings")`.
#'
#' @inheritParams solutionResultsSidebarPane
#'
#' @inherit solutionResultsSidebarPane details return
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("newSolutionSidebarPane")
#' }
#' }
#' @export
newSolutionSidebarPane <- function(id, solutionSettingsId = NULL) {
  # assert arguments are valid
  if (is.null(solutionSettingsId)) {
    solutionSettingsId <- paste0(id, "_settings")
  }
  assertthat::assert_that(
    ### id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ## solutionSettingsId
    assertthat::is.string(solutionSettingsId),
    assertthat::noNA(solutionSettingsId)
  )

  # create sidebar widget
  ## create sidebar
  w <-
    leaflet.extras2::sidebar_pane(
      title = "New solution",
      id = id,
      icon = NULL,
      ### container
      htmltools::tags$div(
        class = "sidebar-pane-content",
        htmltools::tags$div(
          class = "new-solution-pane",
          htmltools::tags$div(
            class = "widget-container",
            solutionSettingsOutput(solutionSettingsId, height = "100%")
          )
        )
      )
    )

  # return result
  w
}
