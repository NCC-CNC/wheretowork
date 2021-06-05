function(input, output, session) {
  # initialize solution settings widget
  output$newSolutionPane_settings <- renderSolutionSettings({
    solutionSettings(ss)
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
