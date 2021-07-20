#' Sever function: import builtin data
#'
#' Set behavior for importing projects using builtin option.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_import_builtin_data)
#' ```
#'
#' @noRd
server_import_builtin_data <- quote({

  # set behavior for importing data using the builtin option
  shiny::observeEvent(input$importModal_builtin_button, {
    ## specify dependencies
    shiny::req(input$importModal_builtin_button)
    shiny::req(input$importModal_name)

    ## update import button
    disable_html_element("importModal_builtin_button")

    ## import configuration
    x <- try(
      read_project(
        path = input$importModal_name,
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
        content = "Something went wrong importing this project",
        style = "danger",
        dismiss = TRUE,
        append = FALSE
      )

      ## reset import button
      shinyFeedback::resetLoadingButton("importModal_builtin_button")
      enable_html_element("importModal_builtin_button")

      ## exit
      return()
    }

    ## import data
    environment(import_data) <- environment()
    import_data(x = x, mode = get_golem_config("mode"))

    ## remove modal
    shiny::removeModal(session)
  })
})
