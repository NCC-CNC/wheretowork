function(input, output, session) {
  # initialize solution settings widget
  output$widget <- renderSolutionResults({
    s <- lapply(
      seq_len(5),
      function(x) simulate_solution(sim_data$themes, sim_data$weights))
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
