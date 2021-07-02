#' @include internal.R solutionSettings.R
NULL

#' Map manager sidebar pane
#'
#' Constructs a sidebar pane for managing the layers on a
#' [leaflet::leafletOutput()] map.
#'
#' @param mapManagerId `character` identifier for the
#'   [mapManager()] widget to create within the sidebar pane.
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
#'   runExample("mapManagerSidebarPane")
#' }
#' }
#'
#' @export
mapManagerSidebarPane <- function(
  id,
  mapManagerId = paste0(id, "_settings")) {
  # assert arguments are valid
  assertthat::assert_that(
    ### id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ### mapManagerId
    assertthat::is.string(mapManagerId),
    assertthat::noNA(mapManagerId))

  # create sidebar widget
  ## create sidebar
  w <-
    leaflet.extras2::sidebar_pane(
      title = "Table of contents",
      id = id,
      icon = NULL,
        htmltools::tags$div(
          class = "sidebar-pane-content",
          htmltools::tags$div(
            class = "map-manager-pane",
            htmltools::tags$div(
              class = "widget-container",
              mapManagerOutput(mapManagerId, height = "100%")
            )
          )
      )
    )

  # return result
  w

}
