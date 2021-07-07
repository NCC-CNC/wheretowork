#' @include internal.R solutionResults.R
NULL

#' Solution results modal
#'
#' Constructs a modal for displaying solution results.
#'
#' @param id `character` identifier for the sidebar pane.
#'
#' @return A `shiny.tag` object with the modal
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("solutionResultsModal")
#' }
#' }
#'
#' @export
solutionResultsModal <- function(id, trigger) {
  # create modal
  out <-
    htmltools::tags$div(
      includeCSS(
        system.file(
          "htmlwidgets",
          "solutionResultsModal.css",
          package = "locationmisc"
        )
      ),
      class = "modal sbs-modal fade",
      id = id,
      tabindex = "-1",
      `data-sbs-trigger` = trigger,
      htmltools::tags$div(
        class = "modal-dialog modal-lg",
        htmltools::tags$div(
          class = "modal-content",
          ## header
          htmltools::tags$div(
            class = "modal-header",
            ### close button
            htmltools::tags$button(
              type = "button",
              class = "close",
              `data-dismiss` = "modal",
              htmltools::tags$span(htmltools::HTML("&times;"))
            ),
            ### title with select input
            htmltools::tags$h4(
              class = "modal-title",
              "Solution results"
            )
          ),
          ## body
          htmltools::tags$div(
            class = "modal-body",
            ### select container
            htmltools::tags$div(
              class = "select_container",
              shiny::selectInput(
                inputId = paste0(id, "_select"),
                choices = "NA",
                label = "Show solution:",
                width = "50px"
              )
            ),
            ### table container
            htmltools::tags$div(
              class = "table_container",
              DT::DTOutput(outputId = paste0(id, "_table"))
            )
          )
        )
      )
    )

  # attach dependencies
  htmltools::attachDependencies(out,
    htmltools::htmlDependencies(shinyBS::bsModal("x", "y", "z")))
}
