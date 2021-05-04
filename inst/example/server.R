library(locationmisc)

function(input, output, session) {
  # weightFactor widget
  ## initialize widget
  output$weightFactor_widget <- renderWeightFactor({
    weightFactor(
      name = "Spatial fragmentation", id = "asdf", initial_factor = 30)
  })
  ## update widget variables
  observeEvent(input$weightFactor_control_name_button, {
    updateWeightName(
      session, "weightFactor_widget",
      input$weightFactor_control_name_input)
  })
  observeEvent(input$weightFactor_control_status_button, {
    updateWeightStatus(
      session, "weightFactor_widget",
      input$weightFactor_control_status_input)
  })
  observeEvent(input$weightFactor_control_factor_button, {
    updateWeightFactor(
      session, "weightFactor_widget",
      input$weightFactor_control_factor_input)
  })
  ## update text box based on slider
  observeEvent(input$weightFactor_widget_status, {
    output$weightFactor_show_status <-
      renderText({ input$weightFactor_widget_status })
  })
  observeEvent(input$weightFactor_widget_factor, {
    output$weightFactor_show_factor <-
      renderText({ input$weightFactor_widget_factor })
  })

  # singleThemeGoal widget
  ## initialize widget
  output$singleThemeGoal_widget <- renderSingleThemeGoal({
    singleThemeGoal(
      name = "Intact grassland",
      feature_id = "1234", feature_name = "intact-grassland",
      feature_total_amount = 50, feature_current_held = 1.0,
      feature_initial_goal = 0.3, feature_limit_goal = 0.1,
      feature_units = "km²")
  })
  ## update widget variables
  observeEvent(input$singleThemeGoal_control_name_button, {
    updateThemeName(
      session, "singleThemeGoal_widget",
      input$singleThemeGoal_control_name_input)
  })
  observeEvent(input$singleThemeGoal_control_status_button, {
    updateThemeStatus(
      session, "singleThemeGoal_widget",
      input$singleThemeGoal_control_status_input)
  })
  observeEvent(input$singleThemeGoal_control_goal_button, {
    updateThemeFeatureGoals(
      session, "singleThemeGoal_widget",
      input$singleThemeGoal_control_goal_input)
  })
  ## update text box based on slider
  observeEvent(input$singleThemeGoal_widget_status, {
    output$singleThemeGoal_show_status <-
      renderText({ input$singleThemeGoal_widget_status })
  })
  observeEvent(input$singleThemeGoal_widget_goal, {
    output$singleThemeGoal_show_goal <-
      renderText({ input$singleThemeGoal_widget_goal })
  })

  # multiThemeGoal widget
  ## initialize widget
  output$multiThemeGoal_widget <- renderMultiThemeGoal({
    multiThemeGoal(
      name = "Ecosystems",
      feature_id = c("F1", "F2", "f3"),
      feature_name = c("forest", "shrubland", "grassland"),
      feature_total_amount = c(50, 20, 90),
      feature_current_held = c(0.0, 0.0, 0.0),
      feature_initial_goal = c(0.0, 0.0, 0.0),
      feature_limit_goal = c(0.0, 0.0, 0.0),
      feature_units = "km²")
  })
  ## update widget variables
  observeEvent(input$multiThemeGoal_control_name_button, {
    updateThemeName(
      session, "multiThemeGoal_widget",
      input$multiThemeGoal_control_name_input)
  })
  observeEvent(input$multiThemeGoal_control_status_button, {
    updateThemeStatus(
      session, "multiThemeGoal_widget",
      input$multiThemeGoal_control_status_input)
  })
  observeEvent(input$multiThemeGoal_control_view_button, {
    updateThemeView(
      session, "multiThemeGoal_widget",
      input$multiThemeGoal_control_view_input)
  })
  ### group view
  observeEvent(input$multiThemeGoal_control_group_goal_button, {
    updateThemeGroupGoal(
      session, "multiThemeGoal_widget",
      input$multiThemeGoal_control_group_goal_input)
  })
  ## single view
  observeEvent(input$multiThemeGoal_control_feature_goal_button, {
    updateThemeFeatureGoals(
      session, "multiThemeGoal_widget",
      c(input$multiThemeGoal_control_F1_goal_input,
        input$multiThemeGoal_control_F2_goal_input,
        input$multiThemeGoal_control_F3_goal_input))
  })
  observeEvent(input$multiThemeGoal_control_feature_status_button, {
    updateThemeFeatureStatuses(
      session, "multiThemeGoal_widget",
      c(input$multiThemeGoal_control_F1_status_input,
        input$multiThemeGoal_control_F2_status_input,
        input$multiThemeGoal_control_F3_status_input))
  })
  ## update text box based on sliders
  observeEvent(input$multiThemeGoal_widget_status, {
    output$multiThemeGoal_show_status <-
      renderText({ input$multiThemeGoal_widget_status })
  })
  observeEvent(input$multiThemeGoal_widget_goal, {
    output$multiThemeGoal_show_goal <-
      renderText({ input$multiThemeGoal_widget_goal })
  })


  # multiThemeGoal widget of data
  ## initialize widget
  output$multiThemeGoal2_widget <- renderMultiThemeGoal({
    multiThemeGoal(
      name = "Vegetation communities",
      feature_id = paste0("FID", seq_len(20)),
      feature_name = paste("Class", seq_len(20)),
      feature_total_amount = rep(100, 20),
      feature_current_held = rep(0.95, 20),
      feature_initial_goal = rep(0.95, 20),
      feature_limit_goal = rep(0.01, 20),
      feature_units = "km²")
  })

  # data data widget demo
  ## TODO

  # solution data widget demo
  ## TODO
}
