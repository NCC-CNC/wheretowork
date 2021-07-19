#' Sever function: verify spatial uploads
#'
#' Set behavior for verifying files uploaded using the spatial data upload
#' option.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_verify_spatial_uploads)
#' ```
#'
#' @noRd
server_verify_spatial_uploads <- quote({

  shiny::observeEvent(input$importModal_spatial_spatial_file, {
    ## specify dependencies
    shiny::req(input$importModal_spatial_spatial_file)
    ## extract file path
    f <- prepare_fileInput(input$importModal_spatial_spatial_file)
    ## check if valid
    v <- is_valid_spatial_file(f)
    ## reset feedback state
    shinyFeedback::hideFeedback(
      inputId = "importModal_spatial_spatial_file"
    )
    ## validate file format
    if (!isTRUE(v)) {
      ### display feedback on file input
      shinyFeedback::showFeedbackDanger(
        inputId = "importModal_spatial_spatial_file",
        text = v
      )
      ### update variable to indicate no valid configuration file
      app_data$spatial_path <- NULL
      ### disable import button
      disable_html_element("importModal_spatial_button")
      ## exit
      return()
    }
    ## update app state given with shapefile path
    app_data$spatial_path <- f[endsWith(f, ".shp")]
    ## display feedback on file input
    shinyFeedback::showFeedbackSuccess(
      inputId = "importModal_spatial_spatial_file"
    )
    ## add columns to spatial settings
    updateImportSettings(
      session = session,
      inputId = "importModal_spatial_settings",
      value = shapefile_field_names(app_data$spatial_path)
    )
    ## enable import button if all files are uploaded
    enable_html_element("importModal_spatial_button")
    # show help text
    shinyjs::showElement("importModal_spatial_text")
  })

})
