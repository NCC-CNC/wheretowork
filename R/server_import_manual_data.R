#' Sever function: import manual data
#'
#' Set behavior for importing projects using manual upload option.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_import_manual_data)
#' ```
#'
#' @noRd
server_import_manual_data <- quote({

  # set behavior for importing data using the manual option
  shiny::observeEvent(input$importModal_manual_button, {
    ## validation
    if (!manual_uploads_ready(app_data)) {
      return()
    }

    ## update import button
    disable_html_element("importModal_manual_button")

    ## import configuration
    x <- try(
      read_project(
        path = app_data$configuration_path,
        spatial_path = app_data$spatial_path,
        attribute_path = app_data$attribute_path,
        boundary_path = app_data$boundary_path,
        mode = get_golem_config("mode")
      ),
      silent = TRUE
    )

    ## throw error if needed
    if (inherits(x, c("try-error", "error"))) {
      ## display error and reset import button
      show_import_error(
        x = x,
        configuration_path = app_data$configuration_path,
        button_id = "importModal_manual_button",
        output = output
      )

      ## exit
      return()
    }

    ## import data
    environment(import_data) <- environment()
    import_data(x = x, mode = get_golem_config("mode"))

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
