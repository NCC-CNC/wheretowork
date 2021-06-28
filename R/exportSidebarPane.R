#' @include internal.R
NULL

#' Export sidebar pane
#'
#' Constructs a sidebar pane for exporting data.
#'
#' @inheritParams importSidebarPane
#'
#' @inherit importSidebarPane details return examples
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

    ## select columns
    shiny::selectInput(
      inputId = paste0(id, "_fields"),
      label = "Select data to save",
      choices = c(),
      multiple = TRUE
    ),

    ## upload button
    shinyBS::bsButton(
      inputId = paste0(id, "_export_button"),
      label = "Save",
      style = "primary",
      icon = shiny::icon("download")
    )
  )
}
