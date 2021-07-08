function(input, output, session) {
  # initialize solution settings widget
  output$widget <- renderSolutionResults({
    solutionResults(x = sols)
  })

  # initialize modal
  ## select input
  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "widget_modal_select",
    choices = sol_names
  )

  ## table
  observeEvent(input$widget_modal_select, {
    ### specify dependencies
    req(input$widget_modal_select)
    if (!input$widget_modal_select %in% sol_names) return()
    ### find solution to show
    i <- which(sol_names == input$widget_modal_select)[[1]]
    ### update tables
    output$widget_modal_themes_table <- renderDataTable({
      sols[[i]]$render_theme_results()
    })
    output$widget_modal_weights_table <- renderDataTable({
      sols[[i]]$render_weight_results()
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
