function(input, output, session) {
  # initialize widget
  output$widget <- renderSolutionResults({
    solutionResults()
  })

  # initialize solutions
  values <- reactiveValues(solutions = list())

  # update widget state
  ## randomly add new solution
  observeEvent(input$add_solution_btn, {
    ### simulate solution
    s <- simulate_solution(themes = sim_data$themes, weights = sim_data$weights)
    ### store solution
    values$solutions <- append(values$solutions, list(s))
    ### add solution to widget
    addSolutionResults(session, "widget", s)
    ## show solution in widget
    showSolutionResults(session, "widget", s$id)
  })

  ## randomly remove existing solution
  observeEvent(input$drop_solution_btn, {
    ### don't try removing a solution if there aren't any
    if (length(values$solutions) >= 1) {
      ### randomly select solution id
      idx <- sample.int(length(values$solutions), 1)
      id <- values$solutions[[idx]]$id
      ### remove solution
      values$solutions <- values$solutions[-idx]
      ### remove solution from widget
      dropSolutionResults(session, "widget", id)
    }
  })

  # update text outputs
  ## text with internal widget state
  output$show <-
    renderText({
      paste(capture.output(
        print(values$solutions), type = "message"), collapse = "\n")
    })

}
