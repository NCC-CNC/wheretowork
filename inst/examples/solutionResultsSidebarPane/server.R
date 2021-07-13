function(input, output, session) {
  # initialize
  ## map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addSidebar(
        id = "sidebar",
        options = list(position = "left", fit = FALSE)
      )
  })

  ## widget
  output$widget <- renderSolutionResults({
    solutionResults(x = sols)
  })

  ## sidebar select input
  # shiny::updateSelectInput(
  #   session = session,
  #   inputId = "widget_select",
  #   choices = sol_names
  # )

  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "widget_select",
    choices = sol_names
  )

  ## modal select input
  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "widget_modal_select",
    choices = sol_names
  )


  ## observers
  ## tables
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

  ## picker input
  observeEvent(input$widget_select, {
    ### specify dependencies
    req(input$widget_select)
    if (!input$widget_modal_select %in% sol_names) return()
    ### show solution results
    showSolutionResults(
      session = session,
      inputId = "widget",
      value = input$widget_select
    )
  })

}
