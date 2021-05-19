#' @include internal.R solutionSettings.R
NULL

#' Map manager sidebar pane
#'
#' Constructs a sidebar pane for managing the layers on a
#' [leaflet::leafletOutput()] map. This is designed to
#' be used as an argument to [leaflet.extras2::sidebar_tabs] when specifying
#' the user interface for a Shiny web application.
#'
#' @param id `character` identifier for the sidebar pane.
#'
#' @param mapManagerId `character` identifier for the
#'   [mapManager()] widget to create within the sidebar pane.
#'   This widget is used to control the settings for new solutions.
#'   Defaults to `paste0(id, "_settings")`.
#'
#' @return A `shiny.tag` object with the sidebar pane.
#'
#' @examples
#' # TODO.
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
      ### container
      htmltools::tags$div(
        class = "map-manager-pane",

        ### settings
        htmltools::tags$div(
          class = "widget-container",
          mapManagerOutput(mapManagerId, height = "100%")
        )

      )
    )

  ## add dependencies
  d <-
    htmltools::htmlDependency(
      name = "mapManagerPane",
      version = "1.0.0",
      src =
        system.file(
          "htmlwidgets", "lib", "mapManagerPane-1.0.0",
          package = "locationmisc"),
      stylesheet = "style.css"
    )
  w <- htmltools::attachDependencies(w, d)

  # return result
  w

}
