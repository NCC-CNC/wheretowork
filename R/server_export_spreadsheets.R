#' Sever function: export spreadsheets
#'
#' Set behavior for buttons used to download spreadsheets.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_export_spreadsheets)
#' ```
#'
#' @noRd
server_export_spreadsheets <- quote({

  # set summary results download button behavior
  output$summary_results_button <- shiny::downloadHandler(
    filename = function() {
      # find selected solution
      i <- which(
        app_data$solution_ids ==
          input$solutionResultsPane_results_modal_select
      )
      # return path based on name of selected solution
      paste0(names(app_data$solution_ids)[i], "_summary_results.xlsx")
    },
    content = function(con) {
      # find selected solution
      i <- which(
        app_data$solution_ids ==
          input$solutionResultsPane_results_modal_select
      )
      # generate table
      x <- as.data.frame(
        app_data$solutions[[i]]$get_summary_results_data(),
        stringsAsFactors = FALSE
      )
      # save table
      write_excel_workbook(x, con)
    },
    contentType = "application/zip"
  )

  # set theme results download button behavior
  output$theme_results_button <- shiny::downloadHandler(
    filename = function() {
      # find selected solution
      i <- which(
        app_data$solution_ids ==
          input$solutionResultsPane_results_modal_select
      )
      # return path based on name of selected solution
      paste0(names(app_data$solution_ids)[i], "_theme_results.xlsx")
    },
    content = function(con) {
      # find selected solution
      i <- which(
        app_data$solution_ids ==
          input$solutionResultsPane_results_modal_select
      )
      # generate table
      x <- as.data.frame(
        app_data$solutions[[i]]$get_theme_results_data(),
        stringsAsFactors = FALSE
      )
      # save table
      write_excel_workbook(x, con)
    },
    contentType = "application/zip"
  )

  # set weight results download button behavior
  output$weight_results_button <- shiny::downloadHandler(
    filename = function() {
      # find selected solution
      i <- which(
        app_data$solution_ids ==
          input$solutionResultsPane_results_modal_select
      )
      # return path based on name of selected solution
      paste0(names(app_data$solution_ids)[i], "_weight_results.xlsx")
    },
    content = function(con) {
      # find selected solution
      i <- which(
        app_data$solution_ids ==
          input$solutionResultsPane_results_modal_select
      )
      # generate table
      x <- as.data.frame(
        app_data$solutions[[i]]$get_weight_results_data(),
        stringsAsFactors = FALSE
      )
      # save table
      write_excel_workbook(x, con)
    },
    contentType = "application/zip"
  )

  # set include results download button behavior
  output$include_results_button <- shiny::downloadHandler(
    filename = function() {
      # find selected solution
      i <- which(
        app_data$solution_ids ==
          input$solutionResultsPane_results_modal_select
      )
      # return path based on name of selected solution
      paste0(names(app_data$solution_ids)[i], "_include_results.xlsx")
    },
    content = function(con) {
      # find selected solution
      i <- which(
        app_data$solution_ids ==
          input$solutionResultsPane_results_modal_select
      )
      # generate table
      x <- as.data.frame(
        app_data$solutions[[i]]$get_include_results_data(),
        stringsAsFactors = FALSE
      )
      # save table
      write_excel_workbook(x, con)
    },
    contentType = "application/zip"
  )

})
