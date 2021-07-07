function(input, output, session) {
  # initialize solution settings widget
  output$widget <- renderSolutionResults({
    solutionResults(x = sols)
  })

  # initialize modal
  ## select input
  updateSelectInput(
    "widget_modal_select",
    choices = sol_names,
    selected = sol_names[[1]]
  )

  ## table
  observeEvent(input$widget_modal_select, {
    req(input$widget_modal_select)
    i <- which(sol_names == input$widget_modal_select)[[1]]
    output$widget_modal_theme_table <- renderDataTable({
      sol[[i]]$render_themes_table()
    })
    output$widget_modal_weights_table <- renderDataTable({
      sol[[i]]$render_themes_table()
    })
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
