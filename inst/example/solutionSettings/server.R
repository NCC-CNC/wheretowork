function(input, output, session) {
  # initialize widget
  output$widget <- renderSolutionSettings({
    solutionSettings(ss)
  })

  # update widget state
  ## singleTheme
  # observeEvent(input$st_name_button, {
  #   updateThemeName(
  #     session, "widget",
  #     input$st_name_input)
  # })
  # observeEvent(input$st_status_button, {
  #   updateThemeFeatureStatus(
  #     session, "widget",
  #     input$st_status_input)
  # })
  # observeEvent(input$st_goal_button, {
  #   updateThemeFeatureGoal(
  #     session, "widget",
  #     input$st_goal_input)
  # })

  ## multiTheme
  # observeEvent(input$mt_name_button, {
  #   updateThemeName(
  #     session, "widget",
  #     input$mt_name_input)
  # })
  # observeEvent(input$mt_status_button, {
  #   updateThemeFeatureStatus(
  #     session, "widget",
  #     c(input$mt1_status_input, input$mt2_status_input))
  # })
  # observeEvent(input$mt_goal_button, {
  #   updateThemeFeatureGoal(
  #     session, "widget",
  #     c(input$mt1_goal_input, input$mt2_goal_input))
  # })

  ## update internal object based on widget
  # observeEvent(input$widget, {
  #   ss$set_parameter(input$widget)
  # })

  ## update text box based on widget status
  observeEvent(input$widget, {
    output$message <-
      renderText({ paste(capture.output(input$widget), collapse = "\n") })
  })
  observeEvent(input$widget, {
    output$show <-
      renderText({ paste(capture.output(print(ss)), collapse = "\n") })
  })

}
