#' Sever function: verify manual uploads
#'
#' Set behavior for verifying files uploaded using the manual upload option.
#'
#' @param input,output,session Arguments inherited from [shiny::shinyServer].
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_verify_manual_uploads)
#' ```
#'
#' @noRd
server_verify_manual_uploads <- quote({

  # parameter file upload
  shiny::observeEvent(input$importModal_manual_configuration_file, {
    ## specify dependencies
    shiny::req(input$importModal_manual_configuration_file)
    ## extract file path
    f <- prepare_fileInput(input$importModal_manual_configuration_file)
    ## check if valid
    v <- is_valid_configuration_file(f)
    ## reset feedback state
    shinyFeedback::hideFeedback(
      inputId = "importModal_manual_configuration_file"
    )
    if (!isTRUE(v)) {
      ### display feedback on file input
      shinyFeedback::showFeedbackDanger(
        inputId = "importModal_manual_configuration_file",
        text = v
      )
      ### update variable to indicate no valid configuration file
      app_data$configuration_path <- NULL
      ### disable import button
      disable_html_element("importModal_manual_button")
      ## exit
      return()
    }
    ### update app state given valid configuration file
    app_data$configuration_path <- f
    ### display feedback on file input
    shinyFeedback::showFeedbackSuccess(
      inputId = "importModal_manual_configuration_file"
    )
    ### enable import button if all files are uploaded
    if (
      is.character(app_data$configuration_path) &&
      is.character(app_data$spatial_path) &&
      is.character(app_data$attribute_path) &&
      is.character(app_data$boundary_path)) {
      enable_html_element("importModal_manual_button")
    }
  })

  # spatial data upload
  shiny::observeEvent(input$importModal_manual_spatial_file, {
    ## specify dependencies
    shiny::req(input$importModal_manual_spatial_file)
    ## extract file path
    f <- prepare_fileInput(input$importModal_manual_spatial_file)
    ## check if valid
    v <- is_valid_spatial_file(f)
    ## reset feedback state
    shinyFeedback::hideFeedback(inputId = "importModal_manual_spatial_file")
    ## validate file format
    if (!isTRUE(v)) {
      ### display feedback on file input
      shinyFeedback::showFeedbackDanger(
        inputId = "importModal_manual_spatial_file",
        text = v
      )
      ### update variable to indicate no valid configuration file
      app_data$spatial_path <- NULL
      ### disable import button
      disable_html_element("importModal_manual_button")
      ## exit
      return()
    }
    ### update app state given valid configuration file
    if (any(endsWith(f, ".shp"))) {
      app_data$spatial_path <- f[endsWith(f, ".shp")]
    } else {
      app_data$spatial_path <- f[[1]]
    }
    ### display feedback on file input
    shinyFeedback::showFeedbackSuccess(
      inputId = "importModal_manual_spatial_file"
    )
    ### enable import button if all files are uploaded
    if (
      is.character(app_data$configuration_path) &&
      is.character(app_data$spatial_path) &&
      is.character(app_data$attribute_path) &&
      is.character(app_data$boundary_path)) {
      enable_html_element("importModal_manual_button")
    }
  })

  # attribute data upload
  shiny::observeEvent(input$importModal_manual_attribute_file, {
    ## specify dependencies
    shiny::req(input$importModal_manual_attribute_file)
    ## extract file path
    f <- prepare_fileInput(input$importModal_manual_attribute_file)
    ## check if valid
    v <- is_valid_attribute_file(f)
    ## reset feedback state
    shinyFeedback::hideFeedback(
      inputId = "importModal_manual_attribute_file"
    )
    ## validate file format
    if (!isTRUE(v)) {
      ### display feedback on file input
      shinyFeedback::showFeedbackDanger(
        inputId = "importModal_manual_attribute_file",
        text = v
      )
      ### update variable to indicate no valid configuration file
      app_data$attribute_path <- NULL
      ### disable import button
      disable_html_element("importModal_manual_button")
      ## exit
      return()
    }
    ### update app state given valid configuration file
    app_data$attribute_path <- f
    ### display feedback on file input
    shinyFeedback::showFeedbackSuccess(
      inputId = "importModal_manual_attribute_file"
    )
    ### enable import button if all files are uploaded
    if (
      is.character(app_data$configuration_path) &&
      is.character(app_data$spatial_path) &&
      is.character(app_data$attribute_path) &&
      is.character(app_data$boundary_path)) {
      enable_html_element("importModal_manual_button")
    }
  })

  # boundary data upload
  shiny::observeEvent(input$importModal_manual_boundary_file, {
    ## specify dependencies
    shiny::req(input$importModal_manual_boundary_file)
    ## extract file path
    f <- prepare_fileInput(input$importModal_manual_boundary_file)
    ## check if valid
    v <- is_valid_boundary_file(f)
    ## reset feedback state
    shinyFeedback::hideFeedback(
      inputId = "importModal_manual_boundary_file"
    )
    ## validate file format
    if (!isTRUE(v)) {
      ### display feedback on file input
      shinyFeedback::showFeedbackDanger(
        inputId = "importModal_manual_boundary_file",
        text = v
      )
      ### update variable to indicate no valid configuration file
      app_data$boundary_path <- NULL
      ### disable import button
      disable_html_element("importModal_manual_button")
      ## exit
      return()
    }
    ### update app state given valid configuration file
    app_data$boundary_path <- f
    ### display feedback on file input
    shinyFeedback::showFeedbackSuccess(
      inputId = "importModal_manual_boundary_file"
    )
    ### enable import button if all files are uploaded
    if (
      is.character(app_data$configuration_path) &&
      is.character(app_data$spatial_path) &&
      is.character(app_data$attribute_path) &&
      is.character(app_data$boundary_path)) {
      enable_html_element("importModal_manual_button")
    }
  })

})
