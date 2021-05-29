function(input, output, session) {
  # initialize widget
  output$widget <- renderMapManager({
    mapManager(ss)
  })

  # update widget state
  ## singleTheme
  observeEvent(input$st_name_button, {
    updateMapManager(
      session, "widget",
      list(
        id = "SPECIES", parameter = "name",
        value = input$st_name_input)
    )
  })
  observeEvent(input$st_visible_button, {
    updateMapManager(
      session, "widget",
      list(
        id = "SPECIES", parameter = "visible",
        value = input$st_visible_input)
    )
  })

  ## multiTheme
  observeEvent(input$mt_name_button, {
    updateMapManager(
      session, "widget",
      list(
        id = "ER", parameter = "name",
        value = input$mt_name_input)
    )
  })
  observeEvent(input$mt_visible_button, {
    updateMapManager(
      session, "widget",
      list(
        id = "ER", parameter = "visible",
        value = input$mt_visible_input)
    )
  })
  observeEvent(input$mt_feature_visible_button, {
    updateMapManager(
      session, "widget",
      list(
        id = "ER", parameter = "visible",
        value =
          c(input$mt1_feature_visible_input, input$mt2_feature_visible_input))
    )
  })

  ## weight
  observeEvent(input$w_name_button, {
    updateMapManager(
      session, "widget",
      list(
        id = "HFP", parameter = "name",
        value = input$w_name_input)
    )
  })
  observeEvent(input$w_visible_button, {
    updateMapManager(
      session, "widget",
      list(
        id = "HFP", parameter = "visible",
        value = input$w_visible_input)
    )
  })

  # update internal object based on widget
  observeEvent(input$widget, {
    ss$set_parameter(input$widget)
  })

  # update text outputs
  ## text with messages from widget
  observeEvent(input$widget, {
    output$message <-
      renderText({ paste(capture.output(input$widget), collapse = "\n") })
  })
  ## text with internal widget state
  observeEvent(input$widget, {
    output$show <-
      renderText({
        paste(capture.output(print(ss), type = "message"), collapse = "\n")
      })
  })

}
