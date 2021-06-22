function(input, output, session) {
  # initialize solution settings widget
  output$mapManagerPane_settings <- renderMapManager({
    mapManager(mm)
  })

  # initialize map
  output$map <- renderLeaflet({
    mm$get_initial_map(d) %>%
    addSidebar(
      id = "sidebar",
      options = list(position = "left", fit = FALSE)
    )
  })

  # update map
  observeEvent(input$mapManagerPane_settings, {
    mm$set_parameter(input$mapManagerPane_settings)
    if (identical(input$mapManagerPane_settings$parameter, "remove")) {
      dropMapManagerLayer(
        session, "mapManagerPane_settings", input$mapManagerPane_settings$id)
    }
    mm$update_map(leaflet::leafletProxy("map"))
  })

}
