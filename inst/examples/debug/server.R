function(input, output, session) {
  # initialize solution settings widget
  output$mapManagerPane_settings <- renderMapManager({
    mapManager(mm)
  })

  # initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addSidebar(
        id = "sidebar",
        options = list(position = "left", fit = FALSE)
      )
  })

}
