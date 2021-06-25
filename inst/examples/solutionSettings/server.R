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
        id = "SPECIES", setting = "name",
        value = input$st_name_input, type = "theme")
    )
  })
  observeEvent(input$st_status_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "SPECIES", setting = "status",
        value = input$st_status_input, type = "theme")
    )
  })
  observeEvent(input$st_goal_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "SPECIES", setting = "feature_goal",
        value = input$st_goal_input, type = "theme")
    )
  })

  ## multiTheme
  observeEvent(input$mt_name_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", setting = "name",
        value = input$mt_name_input, type = "theme")
    )
  })
  observeEvent(input$mt_status_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", setting = "status",
        value = input$mt_status_input, type = "theme")
    )
  })
  observeEvent(input$mt_view_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", setting = "view",
        value = input$mt_view_input, type = "theme")
    )
  })
  observeEvent(input$mt_group_goal_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", setting = "group_goal",
        value = input$mt_group_goal_input, type = "theme")
    )
  })
  observeEvent(input$mt_feature_status_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", setting = "feature_status",
        value =
          c(input$mt1_feature_status_input, input$mt2_feature_status_input),
        type = "theme")
    )
  })
  observeEvent(input$mt_feature_goal_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "ER", setting = "feature_goal",
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
        id = "HFP", setting = "name",
        value = input$w_name_input, type = "weight")
    )
  })
  observeEvent(input$w_status_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "HFP", setting = "status",
        value = input$w_status_input, type = "weight")
    )
  })
  observeEvent(input$w_factor_button, {
    updateSolutionSettings(
      session, "widget",
      list(
        id = "HFP", setting = "factor",
        value = input$w_factor_input, type = "weight")
    )
  })

  # update internal object based on widget
  observeEvent(input$widget, {
    ss$set_setting(input$widget)
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
      renderText({
        paste(capture.output(print(ss), type = "message"), collapse = "\n")
      })
  })

}
