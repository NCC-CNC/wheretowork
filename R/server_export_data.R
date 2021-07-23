#' Sever function: export data
#'
#' Set download handler for download button.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_export_data)
#' ```
#'
#' @noRd
server_export_data <- quote({

  # disable button if no fields selected
  shiny::observeEvent(input$exportPane_fields, ignoreNULL = FALSE, {
    ## enable/disable if none specified
    if (length(input$exportPane_fields) > 0) {
      shinyjs::enable("exportPane_button")
    } else {
      shinyjs::disable("exportPane_button")
    }
  })

  # set download button behavior
  output$exportPane_button <- shiny::downloadHandler(
    filename = function() {
      paste0("prioritization-", Sys.Date(), ".zip")
    },
    content = function(con) {
      write_spatial_data(
        x = app_data$dataset$get_index(input$exportPane_fields),
        path = con,
        name = "data"
      )
    },
    contentType = "application/zip"
  )

})
