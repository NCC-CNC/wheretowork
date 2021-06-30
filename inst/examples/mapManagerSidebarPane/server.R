function(input, output, session) {
  # initialize solution settings widget
  output$mapManagerPane_settings <- renderMapManager({
    mapManager(mm)
  })

  # initialize map
  output$map <- renderLeaflet({
    leaflet_map() %>%
    leaflet::flyToBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax) %>%
    leaflet::fitBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax) %>%
    leaflet.extras2::addSidebar(
        id = "sidebar",
        options = list(position = "left", fit = FALSE)) %>%
    {mm$initialize_map(.)}
  })

  # update map
  observeEvent(input$mapManagerPane_settings, {
    req(input$mapManagerPane_settings)
    mm$set_setting(input$mapManagerPane_settings)
    if (identical(input$mapManagerPane_settings$setting, "remove")) {
      dropMapManagerLayer(
        session, "mapManagerPane_settings", input$mapManagerPane_settings$id)
    }
    mm$update_map(leaflet::leafletProxy("map"))
  })

}
