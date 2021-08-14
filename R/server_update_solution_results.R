#' Sever function: update solution results
#'
#' Set behavior for updating the solution results sidebar content.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_update_solution_results)
#' ```
#'
#' @noRd
server_update_solution_results <- quote({

  # update solution results sidebar content
  shiny::observeEvent(input$solutionResultsPane_results_select, {
    ## specify dependencies
    shiny::req(input$solutionResultsPane_results_select)
    if (
      !input$solutionResultsPane_results_select %in% app_data$solution_ids) {
      return()
    }
    ## show solution results
    showSolutionResults(
      session = session,
      inputId = "solutionResultsPane_results",
      value = input$solutionResultsPane_results_select
    )
  })

  # update tables in solution modal
  shiny::observeEvent(input$solutionResultsPane_results_modal_select, {
    ## specify dependencies
    if (
      !input$solutionResultsPane_results_select %in% app_data$solution_ids) {
      return()
    }

    ## find selected solution
    i <- which(
      app_data$solution_ids == input$solutionResultsPane_results_modal_select
    )
    ## render summary results
    output$solutionResultsPane_results_modal_summary_table <-
      DT::renderDT({
        app_data$solutions[[i]]$render_summary_results()
      })
    ## render theme results
    output$solutionResultsPane_results_modal_themes_table <-
      DT::renderDT({
        app_data$solutions[[i]]$render_theme_results()
      })
    ## render weight results
    output$solutionResultsPane_results_modal_weights_table <-
      DT::renderDT({
        app_data$solutions[[i]]$render_weight_results()
      })
    ## render include results
    output$solutionResultsPane_results_modal_includes_table <-
      DT::renderDT({
        app_data$solutions[[i]]$render_include_results()
      })
  })
})
