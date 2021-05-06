function(input, output, session) {
  # initialize widget
  output$widget <- renderSolutionSettings({
    solutionSettings(ss)
  })

  # update widget state
  ## singleTheme
  observeEvent(input$st_name_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "SPECIES", parameter = "name",
        value = input$st_name_input, type = "theme")
    )
  })
  observeEvent(input$st_status_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "SPECIES", parameter = "status",
        value = input$st_status_input, type = "theme")
    )
  })
  observeEvent(input$st_goal_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "SPECIES", parameter = "feature_goal",
        value = input$st_goal_input, type = "theme")
    )
  })

  ## multiTheme
  observeEvent(input$mt_name_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", parameter = "name",
        value = input$mt_name_input, type = "theme")
    )
  })
  observeEvent(input$mt_status_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", parameter = "status",
        value = input$mt_status_input, type = "theme")
    )
  })
  observeEvent(input$mt_view_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", parameter = "view",
        value = input$mt_view_input, type = "theme")
    )
  })
  observeEvent(input$mt_group_goal_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", parameter = "group_goal",
        value = input$mt_group_goal_input, type = "theme")
    )
  })
  observeEvent(input$mt_feature_status_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", parameter = "feature_status",
        value =
          c(input$mt1_feature_status_input, input$mt2_feature_status_input),
        type = "theme")
    )
  })
  observeEvent(input$mt_feature_goal_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", parameter = "feature_goal",
        value = c(input$mt1_feature_goal_input, input$mt2_feature_goal_input),
        type = "theme")
    )
  })

  ## weight
  ## singleTheme
  observeEvent(input$w_name_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "HFP", parameter = "name",
        value = input$w_name_input, type = "weight")
    )
  })
  observeEvent(input$w_status_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "HFP", parameter = "status",
        value = input$w_status_input, type = "weight")
    )
  })
  observeEvent(input$w_factor_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "HFP", parameter = "factor",
        value = input$w_factor_input, type = "weight")
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
  ## text with interal widget state
  observeEvent(input$widget, {
    output$show <-
      renderText({ paste(capture.output(print(ss)), collapse = "\n") })
  })

}
