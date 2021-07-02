function(input, output, session) {
  # initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addSidebar(
        id = "dataSidebar",
        options = list(position = "left", fit = FALSE)
      ) %>%
      addSidebar(
        id = "analysisSidebar",
        options = list(position = "right", fit = FALSE)
      )
  })

  # initialize widgets
  output$solutionResultsPane_results <- renderSolutionResults({
    solutionResults(list(sim_solution))
  })
  output$mapManagerPane_settings <- renderMapManager({
    mapManager(mm)
  })
  output$newSolutionPane_settings <- renderSolutionSettings({
    solutionSettings(ss)
  })

}
