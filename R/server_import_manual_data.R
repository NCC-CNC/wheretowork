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
    if (
      !is.character(app_data$configuration_path) ||
      !is.character(app_data$spatial_path) ||
      !is.character(app_data$attribute_path) ||
      !is.character(app_data$boundary_path)) {
      return()
    }

    ## update import button
    disable_html_element("importModal_manual_button")

    ## remove alert if needed
    try(
      shinyBS::closeAlert(
        session = session, alertId = "import_error_alert"
      ),
      silent = TRUE
    )

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
    if (inherits(x, "try-error")) {
      ## display error message on import alert
      shinyBS::createAlert(
        session = session,
        anchorId = "importModal_alert",
        alertId = "import_error_alert",
        title = "Oops...",
        content = error_message(x),
        style = "danger",
        dismiss = TRUE,
        append = FALSE
      )

      ## reset import button
      shinyFeedback::resetLoadingButton("importModal_manual_button")
      enable_html_element("importModal_manual_button")

      ## exit
      return()
    }

    ## import data
    environment(import_data) <- environment()
    import_data(x = x, mode = get_golem_config("mode"))

    ## remove modal
    shiny::removeModal(session)

    ## show help modal
    if (identical(app_data$mode, "beginner")) {
      shinyBS::toggleModal(session, modalId = "helpModal", toggle = "open")
    }

  })
})
