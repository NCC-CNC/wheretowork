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
      openxlsx::write.xlsx(x, file = con, overwrite = TRUE)
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
      openxlsx::write.xlsx(x, file = con, overwrite = TRUE)
    },
    contentType = "application/zip"
  )

})
