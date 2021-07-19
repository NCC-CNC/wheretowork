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
    assertthat::noNA(id))

  # create sidebar widget
  ## create sidebar
  leaflet.extras2::sidebar_pane(
    title = "Contact",
    id = id,
    icon = NULL,
    htmltools::tags$div(
      class = "sidebar-pane-content",
      htmltools::tags$div(
        class = "sidebar-pane-inner",
      htmltools::tags$div(
        class = "generic-container",
          htmltools::tags$p(
            class = "text-left",
            "If you have any questions, comments, or concerns with ",
            "using this application, please contact ",
            htmltools::tags$a(
              href = "https://www.richard-schuster.com/",
              target = "_blank",
              "Dr. Richard Schuster",
              .noWS = "outside"
            ),
            " (",
            htmltools::tags$a(
              href = "mailto:richard.schuster@natureconservancy.ca",
              target = "_blank",
              "richard.schuster@natureconservancy.ca",
              .noWS = "outside"
            ),
            "). Additionally, to discuss possible enhancements, ",
            "additional functionality, or customizing this application ",
            "for your work, please contact ",
            htmltools::tags$a(
              href = "https://carleton.ca/bennett-lab/lab-members/",
              target = "_blank",
              "Prof. Joe Bennett",
              .noWS = "outside"
            ),
            " (",
            htmltools::tags$a(
              href = "mailto:JosephBennett@cunet.carleton.ca",
              target = "_blank",
              "JosephBennett@cunet.carleton.ca",
              .noWS = "outside"
            ),
            ").",
            .noWS = c("after-begin", "before-end")
          )
        )
      )
    )
  )
}
