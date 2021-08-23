#' @include internal.R
NULL

#' Import modal
#'
#' Constructs a modal for importing data.
#'
#' @param id `character` identifier.
#'
#' @return A `shiny.tag` object.
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("importModal")
#' }
#' }
#'
#' @export
importModal <- function(id) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(id),
    assertthat::noNA(id)
  )

  # create modal
  shiny::modalDialog(
    title = htmltools::tags$p(
      "Welcome to Where To Work",
      style = "text-align:center"
    ),
    easyClose = FALSE,
    fade = TRUE,
    footer = htmltools::tags$div(
      # styling
      style = "text-align: center",
      # builtin button
      shiny::conditionalPanel(
        condition = paste0("input.", id, "_method == 'builtin'"),
        shinyFeedback::loadingButton(
          inputId = paste0(id, "_builtin_button"),
          label = "Import",
          loadingLabel = "Loading..."
        )
      ),
      # manual button
      shiny::conditionalPanel(
        condition = paste0("input.", id, "_method == 'manual'"),
        shinyFeedback::loadingButton(
          inputId = paste0(id, "_manual_button"),
          label = "Import",
          loadingLabel = "Loading..."
        )
      ),
      # spatial button
      shiny::conditionalPanel(
        condition = paste0("input.", id, "_method == 'spatial'"),
        shinyFeedback::loadingButton(
          inputId = paste0(id, "_spatial_button"),
          label = "Import",
          loadingLabel = "Loading..."
        )
      )
    ),

    ## import method
    shiny::selectInput(
      inputId = paste0(id, "_method"),
      label = "Select import method",
      choices = c(
        "built-in project" = "builtin",
        "upload project data" = "manual",
        "spatial dataset" = "spatial"
      ),
      selected = "built-in project",
      multiple = FALSE
    ),

    ## builtin method
    shiny::conditionalPanel(
      ### condition
      condition = paste0("input.", id, "_method == 'builtin'"),
      ### main
      shiny::selectInput(
        inputId = paste0(id, "_name"),
        label = "Select project",
        choices = c("No built-in projects available" = "NA"),
        multiple = FALSE
      )
    ),

    ## manual method
    shiny::conditionalPanel(
      ### condition
      condition = paste0("input.", id, "_method == 'manual'"),
      ### main
      shiny::fileInput(
        paste0(id, "_manual_configuration_file"),
        "Select configuration file",
        multiple = FALSE,
        accept = ".yaml"
      ),
      shiny::fileInput(
        paste0(id, "_manual_spatial_file"),
        "Select spatial data",
        multiple = TRUE,
        accept = c(".shp", ".shx", ".prj", ".dbf", ".cpg", ".tif")
      ),
      shiny::fileInput(
        paste0(id, "_manual_attribute_file"),
        "Select attribute data",
        multiple = FALSE,
        accept = c(".csv", ".csv.gz")
      ),
      shiny::fileInput(
        paste0(id, "_manual_boundary_file"),
        "Select boundary data",
        multiple = FALSE,
        accept = c(".csv", ".csv.gz")
      )
    ),

    ## spatial method
    shiny::conditionalPanel(
      ### condition
      condition = paste0("input.", id, "_method == 'spatial'"),
      ### main
      shiny::fileInput(
        paste0(id, "_spatial_spatial_file"),
        "Select shapefile",
        multiple = TRUE,
        accept = c(".shp", ".shx", ".prj", ".dbf", ".cpg"),
      ),
      htmltools::tags$label(
        id = paste0(id, "_spatial_text"),
        class = "control-label",
        "Select fields"
      ),
      importSettingsOutput(outputId = paste0(id, "_spatial_settings"))
    ),

    # error alert
    shinyBS::bsAlert(
      anchorId = paste0(id, "_alert")
    )
  )
}
