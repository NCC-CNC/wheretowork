#' @include internal.R widget_solutionResults_ui.R
NULL

#' Solution results modal
#'
#' Constructs a modal for displaying solution results.
#'
#' @param id `character` identifier for the modal.
#'
#' @param trigger `character` identifier for the modal trigger.
#'   See [shinyBS::bsModal] for further details.
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
      # script to style modal backdrop
      htmltools::tags$script(htmltools::HTML(paste0(
        "$('#", id, "').on('show.bs.modal', function() {",
        "  setTimeout(function() {",
        "  $('#", id, "').appendTo('body');",
        "$('.modal-backdrop').addClass('solution-results-modal-backdrop');",
        "})});"
      ))),
      class = "solution-results-modal modal sbs-modal fade",
      id = id,
      tabindex = "-1",
      `data-sbs-trigger` = trigger,
      # modal content
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
                horizontalPickerInput(
                  inputId = paste0(id, "_select"),
                  choices = "NA",
                  label = "Solution:",
                  multiple = FALSE
                )
              ),
              ### radio button container
              htmltools::tags$div(
                class = "radio-container",
                shinyWidgets::radioGroupButtons(
                  inputId = paste0(id, "_radio"),
                  label = NULL,
                  choices = c(
                    `<i class='fa fa-chart-line'></i>Summary` = "summary",
                    `<i class='fa fa-star'></i>Themes` = "themes",
                    `<i class='fa fa-weight-hanging'></i>Weights` = "weights",
                    `<i class='fa fa-lock'></i>Includes` = "includes"
                  ),
                  justified = FALSE,
                  size = "normal"
                ),
              ),
              ## download buttons
              shiny::downloadButton(
                outputId = "summary_results_button",
                label = "Download summary results",
                class = "btn-sm"
              ),
              shiny::downloadButton(
                outputId = "theme_results_button",
                label = "Download theme results",
                class = "btn-sm"
              ),
              shiny::downloadButton(
                outputId = "weight_results_button",
                label = "Download weight results",
                class = "btn-sm"
              ),
              shiny::downloadButton(
                outputId = "include_results_button",
                label = "Download include results",
                class = "btn-sm"
              )
            )
          ),
          ## body
          htmltools::tags$div(
            class = "modal-body",
            shiny::conditionalPanel(
              condition = paste0("input.", id, "_radio == 'summary'"),
              DT::DTOutput(outputId = paste0(id, "_summary_table"))
            ),
            shiny::conditionalPanel(
              condition = paste0("input.", id, "_radio == 'themes'"),
              DT::DTOutput(outputId = paste0(id, "_themes_table"))
            ),
            shiny::conditionalPanel(
              condition = paste0("input.", id, "_radio == 'weights'"),
              DT::DTOutput(outputId = paste0(id, "_weights_table"))
            ),
            shiny::conditionalPanel(
              condition = paste0("input.", id, "_radio == 'includes'"),
              DT::DTOutput(outputId = paste0(id, "_includes_table"))
            )
          )
        )
      )
    )

  # attach dependencies
  htmltools::attachDependencies(
    out,
    htmltools::htmlDependencies(shinyBS::bsModal("x", "y", "z"))
  )
}
