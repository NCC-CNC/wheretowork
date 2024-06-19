#' Sever function: import spatial data
#'
#' Set behavior for importing projects using spatial data upload option.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_import_spatial_data)
#' ```
#'
#' @noRd
server_import_spatial_data <- quote({

  # set behavior for importing data using the spatial option
  shiny::observeEvent(input$importModal_spatial_settings, {
    
    ## validation
    shiny::req(input$importModal_spatial_settings)
    if (!is.character(app_data$spatial_path)) {
      return()
    }

    ## update import button
    disable_html_element("importModal_spatial_button")

    ## prepare settings
    settings_data <-
      plyr::rbind.fill(lapply(
        input$importModal_spatial_settings, as.data.frame
      ))
    settings_data <- settings_data[settings_data$import, , drop = FALSE]
    settings_data$type <- tolower(settings_data$type)

    ## verify that at least one theme is selected
    if (!any(settings_data$type == "theme")) {
      ### create alert
      shinyalert::shinyalert(
        title = "Oops...",
        type = "error",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
        confirmButtonCol = "#337ab7",
        text = "Error: at least one theme must be selected",
      )
      ### reset import button
      shinyFeedback::resetLoadingButton("importModal_spatial_button")
      enable_html_element("importModal_spatial_button")
      ### exit
      return()
    }

    ## import spatial data
    rd <- try(
      suppressMessages(sf::read_sf(app_data$spatial_path)),
      silent = TRUE
    )

    ## throw error if import failed needed
    if (inherits(rd, "try-error")) {
      ### create alert
      shinyalert::shinyalert(
        title = "Oops...",
        type = "error",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
        confirmButtonCol = "#337ab7",
        text = "Error: not valid ESRI Shapefile format",
      )
      ### reset import button
      shinyFeedback::resetLoadingButton("importModal_spatial_button")
      enable_html_element("importModal_spatial_button")
      ### exit
      return()
    }

    ## throw error if shapefile contains missing values
    ad <- sf::st_drop_geometry(rd)
    ad <- ad[, settings_data$name, drop = FALSE]
    ad <- setNames(
      vapply(ad, function(x) any(is.na(x)), FUN.VALUE = logical(1)),
      names(ad)
    )
    if (any(ad)) {
      ### display error message on import alert
      shinyalert::shinyalert(
        title = "Oops...",
        type = "error",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
        confirmButtonCol = "#337ab7",
        text = paste0(
          "Error: ESRI Shapefile has missing (NA / NULL) values in the ",
          "column(s): ",
          paste(paste0("\"", names(ad)[which(ad)], "\""), collapse = ", "),
          ".\nAs such, these columns cannot be used to specify Themes, ",
          "Weights, or Includes. Please exclude them from the analysis ",
          "(by un-checking their respective checkboxes), or update ",
          "the ESRI Shapefile so that these columns do not have any missing ",
          " values."
        )
      )
      ### reset import button
      shinyFeedback::resetLoadingButton("importModal_spatial_button")
      enable_html_element("importModal_spatial_button")
      ### exit
      return()
    }
    ## clean up
    rm(ad)

    ## prepare data for import
    ### initialize list
    x <- list()

    ### run mode
    mode <- get_golem_config("mode")
    x$mode <- ifelse(
      identical(mode, "project"),
      "advanced",
      mode
    )
    
    ### set project name
    x$name <- basename(app_data$spatial_path)

    ### generate dataset
    x$dataset <- new_dataset_from_auto(rd) 

    ### generate includes
    x$includes <- lapply(
      settings_data$name[settings_data$type == "include"], function(y) {
        new_include(
          name = y, variable = new_variable_from_auto(x$dataset, index = y),
          visible = FALSE
        )
      }
    )
    
    ### generate excludes
    x$excludes <- lapply(
      settings_data$name[settings_data$type == "exclude"], function(y) {
        new_exclude(
          name = y, variable = new_variable_from_auto(x$dataset, index = y),
          visible = FALSE
        )
      }
    )    

    ### generate weights
    x$weights <- lapply(
      settings_data$name[settings_data$type == "weight"], function(y) {
        new_weight(
          name = y, variable = new_variable_from_auto(x$dataset, index = y),
          status = FALSE,
          visible = FALSE
        )
      }
    )

    ### generate themes
    x$themes <- lapply(
      settings_data$name[settings_data$type == "theme"], function(y) {
        new_theme(
          name = y,
          feature = new_feature(
            name = y,
            variable = new_variable_from_auto(x$dataset, index = y),
            visible = FALSE
          )
        )
      }
    )

    ### calculate current amount held for each theme and weight
    ss <- new_solution_settings(
      themes = x$themes, weights = x$weights, includes = x$includes,
      excludes = x$excludes, parameters = list()
    )
    ss$update_current_held()

    ## import data
    environment(import_data) <- environment()
    import_data(x = x, mode = mode)

    ## remove modal
    shiny::removeModal(session)
    
    # add side-bar spinner
    shinyjs::runjs(
      "const sidebarSpinner = document.createElement('div');
       sidebarSpinner.classList.add('sidebar-spinner');
       const mapManagerPane_settings = document.querySelector('#mapManagerPane_settings');
       mapManagerPane_settings.appendChild(sidebarSpinner);"
    )    

    ## show help modal
    if (identical(app_data$mode, "beginner")) {
      shinyBS::toggleModal(session, modalId = "helpModal", toggle = "open")
    }

  })
})
