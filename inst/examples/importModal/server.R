function(input, output, session) {
  # initialization
  ## import data modal
  shiny::showModal(importModal(id = "importModal"))
  ## leaflet map
  output$map <- renderLeaflet({
    leaflet::leaflet() %>%
    add_leaflet_map_components() %>%
    addSidebar(
      id = "sidebar",
      options = list(position = "left", fit = FALSE)
    )
  })

  # file upload handlers
  ## parameter
  observeEvent(input$importModal_configuration_file, {
    req(input$importModal_configuration_file)
    config_path <<-
      prepare_fileInput_files(input$importModal_configuration_file)
  })

  ## spatial
  observeEvent(input$importModal_spatial_file, {
    req(input$importModal_spatial_file)
    spatial_path <<-
      prepare_fileInput_files(input$importModal_spatial_file)
  })

  ## attribute
  observeEvent(input$importModal_attribute_file, {
    req(input$importModal_attribute_file)
    attribute_path <<-
      prepare_fileInput_files(input$importModal_attribute_file)
  })

  ## boundary
  observeEvent(input$importModal_boundary_file, {
    req(input$importModal_boundary_file)
    boundary_path <<-
      prepare_fileInput_files(input$importModal_boundary_file)
  })

  # import button
  observeEvent(input$importModal_import_button, {
    ## validation
    if (is.null(config_path) ||
        is.null(spatial_path) ||
        is.null(attribute_path) ||
        is.null(boundary_path)) {
      return()
    }
    ## import configuration
    x <- try(
      read_configuration_file(
        x = config_path,
        spatial_path = spatial_path,
        attribute_path = attribute_path,
        boundary_path = boundary_path),
      silent = TRUE
    )
    ## throw error if needed
    if (inherits(x, "try-error")) {
      shinyBS::createAlert(
        session = session,
        anchorId = "importModal_import_alert",
        alertId = "import_alert",
        title = "Oops",
        content = as.character(x),
        append = FALSE)
      return()
    }
    ## store variables
    dataset <<- x$dataset
    themes <<- x$themes
    weights <<- x$weights
    includes <<- x$includes
    mode <<- x$mode
    ## store widget variables
    mm <<- new_map_manager(append(themes, append(weights, includes)))
    ## update map manager
    output$mapManagerPane_settings <- renderMapManager(mapManager(mm))
    ## update leaflet map
    mm$get_initial_map(dataset, leaflet::leafletProxy("map"))
    ## remove modal
    shiny::removeModal(session)
  })

  # update map
  observeEvent(input$mapManagerPane_settings, {
    mm$set_setting(input$mapManagerPane_settings)
    if (identical(input$mapManagerPane_settings$setting, "remove")) {
      dropMapManagerLayer(
        session, "mapManagerPane_settings", input$mapManagerPane_settings$id)
    }
    mm$update_map(leaflet::leafletProxy("map"))
  })

  # export data
  observeEvent(input$exportPane_export_button, {
    # TODO
  })


}
