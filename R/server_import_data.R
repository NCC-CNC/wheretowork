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
  # store variables
  app_data$project_name <- x$name
  app_data$author_name <- x$author_name
  app_data$author_email <- x$author_email
  app_data$wheretowork_version <- x$wheretowork_version
  app_data$prioritizr_version <- x$prioritizr_version
  app_data$dataset <- x$dataset
  app_data$themes <- x$themes
  app_data$weights <- x$weights
  app_data$includes <- x$includes
  app_data$excludes <- x$excludes
  if (
    identical(mode, "project") || is.null(mode)) {
    app_data$mode <- x$mode
  } else {
    app_data$mode <- mode
  }

  # force default weight factors to be 99 under beginner mode
  # this because the beginner mode does not provide widgets to update weights
  if (identical(mode, "beginner") || identical(app_data$mode, "beginner")) {
    for (i in seq_along(app_data$weights)) {
      app_data$weights[[i]]$factor <- -99
    }
  }

  # create parameters
  area_ref_value <- sum(app_data$dataset$get_planning_unit_areas()) / 100
  area_ref_value <- as.numeric(
    units::set_units(units::set_units(area_ref_value, "m^2"), "km^2")
  )
  area_budget_parameter <-
    wheretowork::new_parameter(
      name = "Total area budget",
      status = FALSE,
      value = 1,
      min_value = 1,
      max_value = 100,
      step_value = 1,
      units = "%",
      reference_value = area_ref_value,
      reference_units = stringi::stri_unescape_unicode("km\\u00B2"),
      hide = TRUE,
      disable = FALSE,
      tool_tip = 
        "Specify a maximum percentage of the total area that should be selected 
        in the solution.",
      id = "budget_parameter"
    )

  boundary_gap_parameter <-
    wheretowork::new_parameter(
      name = "Spatial clustering",
      status = FALSE,
      value = 1,
      min_value = 1,
      max_value = 100,
      step_value = 1,
      units = "%",
      hide = TRUE,
      disable = ifelse(shiny::isTruthy(x$dataset$get_boundary_data()), FALSE, TRUE),
      tool_tip =
        "Specify how much effort that should be spent on promoting spatial 
        clustering in the solution. When no budget is specified, this controls 
        the extra amount of land (above what is needed to meet the goals) 
        that should be selected to maximize spatial clustering (as a percentage). 
        When a budget is specified, this controls the percentage of the total 
        area budget that should be dedicated towards promoting spatial 
        clustering.",
      id = "spatial_parameter"
    )
  
  solution_layer_parameter <-
    wheretowork::new_parameter(
      name = "Hide solution layer from map",
      status = ifelse(shiny::isTruthy(x$dataset$get_boundary_data()), FALSE, TRUE),
      hide = TRUE,
      disable = FALSE,
      tool_tip = 
        "Hide the output solution layer from map to avoid app performance loss. 
        Recommended for vector projects with over 10,000 planning units.",
      id = "solution_layer_parameter"
    )
  
  overlap_parameter <-
    wheretowork::new_parameter(
      name = "Override includes",
      status = FALSE,
      hide = TRUE,
      disable = FALSE,
      tool_tip =
        "By default, includes have importance over excludes if there is overlap. 
        Enabling Override includes switches overlap precedence to excludes.",
      id = "overlap_parameter"
    )
  
  fileinput_parameter <-
    wheretowork::new_parameter(
      name = "Update solution settings",
      status = FALSE,
      hide = TRUE,
      disable = FALSE,
      tool_tip =
        "upload a .yaml file downloaded from this project to update
         toggle switches and slider values from a previous optimization run.",
      id = "fileinput_parameter"
    )    

  # store widgets
  app_data$mm <- new_map_manager(
   append(append(app_data$themes, append(app_data$weights, app_data$includes)), app_data$excludes) 
  )
  app_data$ss <- new_solution_settings(
    themes = app_data$themes,
    weights = app_data$weights,
    includes = app_data$includes,
    excludes = app_data$excludes,
    parameters = list(area_budget_parameter, boundary_gap_parameter, 
                      solution_layer_parameter, overlap_parameter, 
                      fileinput_parameter)
  )
  
  # store  data
  app_data$bbox <- x$dataset$get_bbox(native = FALSE, expand = TRUE)
  app_data$theme_data <- app_data$ss$get_theme_data()
  app_data$weight_data <- app_data$ss$get_weight_data()
  app_data$include_data <- app_data$ss$get_include_data()
  app_data$exclude_data <- app_data$ss$get_exclude_data()
  app_data$boundary_data <- app_data$dataset$get_boundary_data()
  app_data$area_data <- app_data$dataset$get_planning_unit_areas()

  # set app mode
  shinyjs::runjs(paste0("document.body.classList.add('", app_data$mode, "');"))
  
  # gurobi toggle switch for staff-advanced and admin user groups ONLY
  if (any(user_groups %in% c("staff-advanced", "admin"))) {
    # do nothing
  } else {
    ## completely remove gurobi toggle switch
    shinyjs::runjs(
      "document.querySelector('.solution-footer-gurobi').remove();"
    )
  }  

  # update map manager sidebar
  output$mapManagerPane_settings <-
    renderMapManager(mapManager(app_data$mm))

  # update new solution sidebar
  output$newSolutionPane_settings <-
    renderSolutionSettings(solutionSettings(app_data$ss))

  # update map
  map <- leaflet::leafletProxy("map")
  leaflet::flyToBounds(
    map, app_data$bbox$xmin, app_data$bbox$ymin,
    app_data$bbox$xmax, app_data$bbox$ymax
  )
  leaflet::fitBounds(
    map, app_data$bbox$xmin, app_data$bbox$ymin,
    app_data$bbox$xmax, app_data$bbox$ymax
  )
  app_data$mm$initialize_map(map)

  # update export field names
  shiny::updateSelectizeInput(
    session = session,
    inputId = "exportPane_fields",
    choices = stats::setNames(
      app_data$mm$get_layer_indices(),
      app_data$mm$get_layer_names()
    )
  )

  # make sidebars visible
  shinyjs::runjs("$('#dataSidebar').css('display','block');")
  shinyjs::runjs("$('#analysisSidebar').css('display','block');")

  # make title visible
  shinyjs::runjs("$('#app_title').css('display','block');")

  # open sidebars
  leaflet.extras2::openSidebar(
    map,
    id = "mapManagerPane", sidebar_id = "dataSidebar"
  )
  leaflet.extras2::openSidebar(
    map,
    id = "newSolutionPane", sidebar_id = "analysisSidebar"
  )

  # remove startup mode
  ## this makes the buttons and scalebar visible
  shinyjs::runjs("document.body.classList.remove('startup');")

  # return success
  invisible(TRUE)
}
