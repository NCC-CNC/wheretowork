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
      class = "solution-results-modal modal sbs-modal fade",
      id = id,
      tabindex = "-1",
      `data-sbs-trigger` = trigger,
      htmltools::tags$div(
        class = "modal-dialog modal-xl",
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
          htmltools::tags$div(
            class = "modal-header-content",
              ### solution select container
              htmltools::tags$div(
                class = "select-container",
                shinyWidgets::pickerInput(
                  inputId = paste0(id, "_select"),
                  choices = "NA",
                  label = "Solution:",
                  multiple = FALSE,
                  width = "fit",
                  inline = TRUE
                )
              ),
              ### radio button container
              htmltools::tags$div(
                class = "radio-container",
                shinyWidgets::radioGroupButtons(
                  inputId = paste0(id, "_radio"),
                  label = NULL,
                  choices = c(
                    `<i class='fa fa-star'></i>Themes` = "themes",
                    `<i class='fa fa-weight-hanging'></i>Weights` = "weights"),
                 justified = FALSE,
                 size  = "normal"
                ),
              ),
              ## download buttons
              shiny::downloadButton(
                outputId = "theme_results_button",
                label = "Download theme results"),
              shiny::downloadButton(
                outputId = "weight_results_button",
                label = "Download weight results")
            )
          ),
          ## body
          htmltools::tags$div(
            class = "modal-body",
            ### themes panel
            shiny::conditionalPanel(
              condition = paste0("input.", id, "_radio == 'themes'"),
              DT::DTOutput(outputId = paste0(id, "_themes_table"))
            ),
            ### weights panel
            shiny::conditionalPanel(
              condition = paste0("input.", id, "_radio == 'weights'"),
              DT::DTOutput(outputId = paste0(id, "_weights_table"))
            )
          )
        )
      )
    )

  # attach dependencies
  htmltools::attachDependencies(out,
    htmltools::htmlDependencies(shinyBS::bsModal("x", "y", "z")))
}
