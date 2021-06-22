function(input, output, session) {
  # initialize solution settings widget
  output$widget <- renderSolutionResults({
    s <- lapply(
      seq_len(5),
      function(x) simulate_solution(d, sim_themes, sim_weights))
    solutionResults(x = s)
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
