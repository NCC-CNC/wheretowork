function(input, output, session) {
  # initialization
  ## solution settings widget
  output$newSolutionPane_settings <- renderSolutionSettings({
    solutionSettings(ss)
  })
  ## leafet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addSidebar(
        id = "sidebar",
        options = list(position = "left", fit = FALSE)
      )
  })

  # update solution settings widget
  observeEvent(input$newSolutionPane_settings, {
    ss$set_setting(input$newSolutionPane_settings)
  })

  # add solution to map when generating new solution
  observeEvent(input$newSolutionPane_settings_button, {
    # generate name for new solution
    n <- uuid::UUIDgenerate()
    # generate solution
    s <- generate_solution(
      n,
      d,
      settings = ss,
      theme_data = theme_data,
      weight_data = weight_data,
      include_data = include_data,
      boundary_data = boundary_data,
      gap = 0.01,
      boundary_gap =
        (ss$get_parameter("P1")$value * ss$get_parameter("P1")$status) / 100
    )
    # add solution to map
    suppressWarnings({
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("solution") %>%
        leaflet::addRasterImage(
          x = s$variable$get_data(),
          opacity = 0.8,
          group = "solution",
          method = "ngb",
          project = FALSE,
          colors = s$variable$legend$get_color_map()
        )
    })
  })

}
