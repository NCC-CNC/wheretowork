function(input, output, session) {
  # initialization
  ## import data modal
  shiny::showModal(importModal(id = "importModal"))
  ## leaflet map
  output$map <- renderLeaflet(leaflet_map())

  # file upload handlers
  ## parameter
  observeEvent(input$importModal_configuration_file, {
    req(input$importModal_configuration_file)
    config_path <<-
      prepare_fileInput(input$importModal_configuration_file)
  })

  ## spatial
  observeEvent(input$importModal_spatial_file, {
    req(input$importModal_spatial_file)
    spatial_path <<-
      prepare_fileInput(input$importModal_spatial_file)
      spatial_path <<- spatial_path[endsWith(spatial_path, ".shp")]
  })

  ## attribute
  observeEvent(input$importModal_attribute_file, {
    req(input$importModal_attribute_file)
    attribute_path <<-
      prepare_fileInput(input$importModal_attribute_file)
  })

  ## boundary
  observeEvent(input$importModal_boundary_file, {
    req(input$importModal_boundary_file)
    boundary_path <<-
      prepare_fileInput(input$importModal_boundary_file)
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
    bbox <<- dataset$get_bbox(native = FALSE, expand = TRUE)
    ## store widget variables
    mm <<- new_map_manager(append(themes, append(weights, includes)))
    ## update map manager
    output$mapManagerPane_settings <- renderMapManager(mapManager(mm))
    ## update leaflet map
    map <- leaflet::leafletProxy("map")
    leaflet::flyToBounds(map, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    leaflet::fitBounds(map, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    leaflet.extras2::addSidebar(
      map, id = "sidebar",
      options = list(position = "left", fit = FALSE))
    mm$initialize_map(map)
    ## update export field names
    shiny::updateSelectizeInput(
      session = session,
      inputId = "exportPane_fields",
      choices = setNames(mm$get_layer_indices(), mm$get_layer_names()))
    ## remove modal
    shiny::removeModal(session)
  })

  # update map
  ## map manager
  observeEvent(input$mapManagerPane_settings, {
    mm$set_setting(input$mapManagerPane_settings)
    if (identical(input$mapManagerPane_settings$setting, "remove")) {
      dropMapManagerLayer(
        session, "mapManagerPane_settings", input$mapManagerPane_settings$id)
    }
    mm$update_map(leaflet::leafletProxy("map"))
  })
  ## home button
  observeEvent(input$home_button, {
    leaflet::flyToBounds(
      leaflet::leafletProxy("map"),
      bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
  })

  # export data
  output$exportPane_button <- shiny::downloadHandler(
    filename = function() {
      paste0("prioritization-", Sys.Date(), ".zip")
    },
    content = function(con) {
      ## extract spatial data
      d <- dataset$get_index(input$exportPane_fields)
      ## save data
      write_spatial_data(x = d, path = con, name = "data")
    },
    contentType = "application/zip"
  )
}
