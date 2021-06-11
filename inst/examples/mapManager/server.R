function(input, output, session) {
  # initialize widget
  output$widget <- renderMapManager({
    mapManager(mm)
  })

  # update widget state
  ## ordering
  observeEvent(input$order_button, {
    updateMapManagerOrder(
      session, "widget",
      value = rev(seq_along(mm$layers))
    )
  })

  ## new solution
  observeEvent(input$new_solution_button, {
    s <- simulate_solution(themes = list(t1, t2), weights = list(w))
    mm$add_layer(s)
    addMapManagerLayer(session, "widget", value = s)
  })

  ## singleTheme
  observeEvent(input$st_name_button, {
    updateMapManagerLayer(
      session, "widget",
      list(
        id = "SPECIES", parameter = "name",
        value = input$st_name_input)
    )
  })
  observeEvent(input$st_visible_button, {
    updateMapManagerLayer(
      session, "widget",
      list(
        id = "SPECIES", parameter = "visible",
        value = input$st_visible_input)
    )
  })

  ## multiTheme
  observeEvent(input$mt_name_button, {
    updateMapManagerLayer(
      session, "widget",
      list(
        id = "ER", parameter = "name",
        value = input$mt_name_input)
    )
  })
  observeEvent(input$mt_visible_button, {
    updateMapManagerLayer(
      session, "widget",
      list(
        id = "ER", parameter = "visible",
        value = input$mt_visible_input)
    )
  })
  observeEvent(input$mt_feature_visible_button, {
    updateMapManagerLayer(
      session, "widget",
      list(
        id = "ER", parameter = "feature_visible",
        value =
          c(input$mt1_feature_visible_input, input$mt2_feature_visible_input))
    )
  })
  observeEvent(input$mt_order_button, {
    updateMapManagerLayer(
      session, "widget",
      list(
        id = "ER", parameter = "feature_order",
        value = rev(seq_along(t2$feature)))
    )
  })

  ## weight
  observeEvent(input$w_name_button, {
    updateMapManagerLayer(
      session, "widget",
      list(
        id = "HFP", parameter = "name",
        value = input$w_name_input)
    )
  })
  observeEvent(input$w_visible_button, {
    updateMapManagerLayer(
      session, "widget",
      list(
        id = "HFP", parameter = "visible",
        value = input$w_visible_input)
    )
  })

  # update internal object based on widget
  observeEvent(input$widget, {
    mm$set_parameter(input$widget)
    if (identical(input$widget$parameter, "remove")) {
      dropMapManagerLayer(session, "widget", input$widget$id)
    }
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
        paste(capture.output(print(mm), type = "message"), collapse = "\n")
      })
  })

}
