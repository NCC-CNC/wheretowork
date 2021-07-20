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

    ## remove alert if needed
    try(
      shinyBS::closeAlert(
        session = session, alertId = "import_error_alert"
      ),
      silent = TRUE
    )

    ## prepare settings
    settings_data <-
      plyr::rbind.fill(lapply(
        input$importModal_spatial_settings, as.data.frame
      ))
    settings_data <- settings_data[settings_data$import, , drop = FALSE]

    ## verify that at least one theme is selected
    if (!any(settings_data$type == "theme")) {
      shinyBS::createAlert(
        session = session,
        anchorId = "importModal_alert",
        alertId = "import_error_alert",
        title = "Oops...",
        content = "Error: at least one theme must be selected",
        style = "danger",
        dismiss = TRUE,
        append = FALSE
      )
      ## reset import button
      shinyFeedback::resetLoadingButton("importModal_spatial_button")
      enable_html_element("importModal_spatial_button")
      ## exit
      return()
    }

    ## import spatial data
    rd <- try(
      suppressMessages(sf::read_sf(app_data$spatial_path)),
      silent = TRUE
    )

    ## throw error if needed
    if (inherits(rd, "try-error")) {
      ## display error message on import alert
      shinyBS::createAlert(
        session = session,
        anchorId = "importModal_alert",
        alertId = "import_error_alert",
        title = "Oops...",
        content = "Error: not valid ESRI Shapefile format",
        style = "danger",
        dismiss = TRUE,
        append = FALSE
      )
      ## reset import button
      shinyFeedback::resetLoadingButton("importModal_spatial_button")
      enable_html_element("importModal_spatial_button")
      ## exit
      return()
    }

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

    ### generate dataset
    x$dataset <- new_dataset_from_auto(rd)

    ### generate includes
    x$includes <- lapply(
      settings_data$name[settings_data$type == "include"], function(y) {
        new_include(
          name = y, variable = new_variable_from_auto(x$dataset, index = y)
        )
      }
    )

    ### generate weights
    x$weights <- lapply(
      settings_data$name[settings_data$type == "weight"], function(y) {
        new_weight(
          name = y, variable = new_variable_from_auto(x$dataset, index = y)
        )
      }
    )

    ### generate themes
    x$themes <- lapply(
      settings_data$name[settings_data$type == "theme"], function(y) {
        new_single_theme(
          name = y,
          feature = new_feature(
            name = y,
            variable = new_variable_from_auto(x$dataset, index = y)
          )
        )
      }
    )

    ## import data
    environment(import_data) <- environment()
    import_data(x = x, mode = mode)

    ## remove modal
    shiny::removeModal(session)
  })
})
