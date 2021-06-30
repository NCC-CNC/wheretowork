#' @include internal.R solutionSettings.R
NULL

#' New solution sidebar pane
#'
#' Constructs a sidebar pane for generating new solutions.
#'
#' @param solutionSettingsId `character` identifier for the
#'   [solutionSettings()] widget to create within the sidebar pane.
#'   This widget is used to control the settings for new solutions.
#'   Defaults to `paste0(id, "_settings")`.
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
newSolutionSidebarPane <- function(
  id,
  solutionSettingsId = paste0(id, "_settings")) {
  # assert arguments are valid
  assertthat::assert_that(
    ### id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ### solutionSettingsId
    assertthat::is.string(solutionSettingsId),
    assertthat::noNA(solutionSettingsId))

  # create sidebar widget
  ## create sidebar
  w <-
    leaflet.extras2::sidebar_pane(
      title = "New solution",
      id = id,
      icon = NULL,
      ### container
      htmltools::tags$div(
        class = "new-solution-pane",
        htmltools::tags$div(
          class = "widget-container",
          solutionSettingsOutput(solutionSettingsId, height = "100%")
        )
      )
    )

  ## add dependencies
  d <-
    htmltools::htmlDependency(
      name = "newSolutionPane",
      version = "1.0.0",
      src =
        system.file(
          "htmlwidgets", "lib", "newSolutionPane-1.0.0",
          package = "locationmisc"),
      stylesheet = "style.css"
    )
  w <- htmltools::attachDependencies(w, d)

  # return result
  w

}
