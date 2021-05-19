#' @include internal.R solutionSettings.R
NULL

#' New solution sidebar pane
#'
#' Constructs a sidebar pane for generating new solutions. This is designed to
#' be used as an argument to [leaflet.extras2::sidebar_tabs] when specifying
#' the user interface for a Shiny web application.
#'
#' @param id `character` identifier for the sidebar pane.
#'
#' @param solutionSettingsId `character` identifier for the
#'   [solutionSettings()] widget to create within the sidebar pane.
#'   This widget is used to control the settings for new solutions.
#'   Defaults to `paste0(id, "_settings")`.
#'
#' @param nameId `character` identifier for the [shiny::textInput()]
#'   widget to create within the sidebar pane.
#'   This text input widget is used to specify the name for new solutions.
#'   Defaults to `paste0(id, "_name")`.
#'
#' @param buttonId `character` identifier for the [shiny::actionButton()]
#'   widget to create within the sidebar pane.
#'   This button widget is used to create new solutions.
#'   Defaults to `paste0(id, "_button")`.
#'
#' @return A `shiny.tag` object with the sidebar pane.
#'
#' @examples
#' # TODO.
#'
#' @export
newSolutionSidebarPane <- function(
  id,
  solutionSettingsId = paste0(id, "_settings"),
  nameId = paste0(id, "_name"),
  buttonId = paste0(id, "_button")) {
  # assert arguments are valid
  assertthat::assert_that(
    ### id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ### solutionSettingsId
    assertthat::is.string(solutionSettingsId),
    assertthat::noNA(solutionSettingsId),
    ### nameId
    assertthat::noNA(nameId),
    assertthat::is.string(nameId),
    ### buttonId
    assertthat::noNA(buttonId),
    assertthat::is.string(buttonId))

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
        ### settings
        htmltools::tags$div(
          class = "widget-container",
          solutionSettingsOutput(solutionSettingsId, height = "100%")
        ),

        ### footer
        htmltools::tags$div(
          class = "new-solution-footer",
          htmltools::tags$div(
            class = "new-solution-footer-name",
            shiny::textInput(
              nameId, NULL,
              value = "",
              width = "120%",
              placeholder = "name for solution"
            ),
          ),
          htmltools::tags$div(
            class = "new-solution-footer-button",
            shinyBS::bsButton(
              buttonId,
              label = "Generate solution",
              icon = NULL,
              style = "primary",
              type = "action"
            )
          )
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
