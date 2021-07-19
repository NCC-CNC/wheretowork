#' Import data
#'
#' Import project data into the app.
#'
#' @param x `list` with project data.
#'
#' @param mode `character` Mode for importing projects.
#'   Available options include: `"beginner"`, `"advanced"`, or `"project"`.
#'   Defaults to `"project"`.
#'
#' @return Invisible `TRUE`.
#'
#' @export
import_data <- function(x, mode) {
  ## store variables
  app_data$dataset <- x$dataset
  app_data$themes <- x$themes
  app_data$weights <- x$weights
  app_data$includes <- x$includes
  if (
    identical(mode, "project") || is.null(mode)) {
    app_data$mode <- x$mode
  } else {
    app_data$mode <- mode
  }

  ## store widgets
  app_data$mm <- new_map_manager(
    append(app_data$themes, append(app_data$weights, app_data$includes))
  )
  app_data$ss <- new_solution_settings(
    themes = app_data$themes,
    weights = app_data$weights,
    includes = app_data$includes,
    parameters = list(area_budget_parameter, boundary_gap_parameter)
  )

  ## store  data
  app_data$bbox <- x$dataset$get_bbox(native = FALSE, expand = TRUE)
  app_data$theme_data <- app_data$ss$get_theme_data()
  app_data$weight_data <- app_data$ss$get_weight_data()
  app_data$include_data <- app_data$ss$get_include_data()
  app_data$boundary_data <- app_data$dataset$get_boundary_data()

  ## set app mode
  shinyjs::runjs(paste0("document.body.classList.add('", app_data$mode, "');"))

  ## update map manager sidebar
  output$mapManagerPane_settings <-
    renderMapManager(mapManager(app_data$mm))

  ## update new solution sidebar
  output$newSolutionPane_settings <-
    renderSolutionSettings(solutionSettings(app_data$ss))

  ## update map
  map <- leaflet::leafletProxy("map")
  leaflet::flyToBounds(
    map, app_data$bbox$xmin, app_data$bbox$ymin,
    app_data$bbox$xmax, app_data$bbox$ymax)
  leaflet::fitBounds(
    map, app_data$bbox$xmin, app_data$bbox$ymin,
    app_data$bbox$xmax, app_data$bbox$ymax)
  app_data$mm$initialize_map(map)

  ## render sidebars on map
  leaflet.extras2::addSidebar(
    map, id = "analysisSidebar",
    options = list(position = "right", fit = FALSE)
  )
  leaflet.extras2::addSidebar(
    map, id = "dataSidebar",
    options = list(position = "left", fit = FALSE)
  )

  ## update export field names
  shiny::updateSelectizeInput(
    session = session,
    inputId = "exportPane_fields",
    choices = stats::setNames(
      app_data$mm$get_layer_indices(),
      app_data$mm$get_layer_names()
    )
  )

  ## open sidebars
  leaflet.extras2::openSidebar(
    map, id = "mapManagerPane", sidebar_id = "dataSidebar"
  )
  leaflet.extras2::openSidebar(
    map, id = "newSolutionPane", sidebar_id = "analysisSidebar"
  )

  ## remove startup mode
  ## this makes the buttons and scalebar visible
  shinyjs::runjs("document.body.classList.remove('startup');")

  ## return success
  invisible(TRUE)
}
