function(input, output, session) {
  output$widget <- renderSolutionSettings({
    solutionSettings(ss)
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addSidebar(
        id = "mysidebarid",
        options = list(position = "left", fit = FALSE)
      )
  })
}
