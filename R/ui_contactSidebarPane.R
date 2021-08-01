#' @include internal.R
NULL

#' Contact sidebar pane
#'
#' Constructs a sidebar pane for displaying contact information.
#'
#' @inheritParams solutionResultsSidebarPane
#'
#' @inherit solutionResultsSidebarPane details return
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("allSidebars")
#' }
#' }
#'
#' @export
contactSidebarPane <- function(id) {
  # assert arguments are valid
  assertthat::assert_that(
    ### id
    assertthat::is.string(id),
    assertthat::noNA(id)
  )

  # create sidebar widget
  ## create sidebar
  leaflet.extras2::sidebar_pane(
    title = "Contact",
    id = id,
    icon = NULL,
    htmltools::tags$div(
      class = "sidebar-pane-content",
      htmltools::tags$script(paste0("
        $('a[href=\"#", id, "\"]').tooltip({
          container: 'body',
          trigger: 'hover',
          placement: 'right',
          title: 'Open sidebar with contact information'
        });
      ")),
      htmltools::tags$div(
        class = "sidebar-pane-inner",
        htmltools::tags$div(
          class = "generic-container",
          shiny::includeMarkdown(
            system.file("app", "text", "contact.md", package = "wheretowork")
          )
        )
      )
    )
  )
}
