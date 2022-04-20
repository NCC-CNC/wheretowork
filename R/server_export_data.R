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
      # create temporary directory to save data
      td <- tempfile()
      dir.create(td, recursive = TRUE, showWarnings = FALSE)
      # save Excel spreadsheets with solution results
      ## find solutions to save
      export_solutions <- vapply(
        app_data$solutions, FUN.VALUE = logical(1), function(x) {
          x$get_layer_index() %in% input$exportPane_fields
      })
      ## save results
      lapply(app_data$solutions[export_solutions], function(x) {
        ### save excel workbook
        write_excel_workbook(
          list(
            Summary = x$get_summary_results_data(),
            Themes = x$get_theme_results_data(),
            Weights = x$get_weight_results_data(),
            Includes = x$get_include_results_data()
          ),
          file.path(td, paste0(x$get_layer_name(), ".xlsx"))
        )
        ### save CSV spreadsheets
        Map(
          write.csv,
          x = list(
            x$get_summary_results_data(),
            x$get_theme_results_data(),
            x$get_weight_results_data(),
            x$get_include_results_data()
          ),
          file = file.path(
            td,
            paste0(
              x$get_layer_name(),
              "_",
              c("summary", "themes", "weights", "includes"),
              ".csv"
            )
          ),
          row.names = FALSE
        )
      })
      # prepare spatial data for export
      ## if we are saving a shapefile, then include an "_index" column
      ## with the planning unit indices
      d <- app_data$dataset$get_index(input$exportPane_fields)
      if (inherits(d, "sf")) {
        d[["index"]] <- app_data$dataset$get_planning_unit_indices()
      }
      # save spatial data
      write_spatial_data(
        x = d,
        dir = td,
        name = "data"
      )
      # zip files
      withr::with_dir(td, utils::zip(con, dir(td), flags = "-qq"))
    },
    contentType = "application/zip"
  )

})
