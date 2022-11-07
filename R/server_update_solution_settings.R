#' Sever function: update solution settings
#'
#' Set behavior for updating the solution settings sidebar content.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_update_solution_settings)
#' ```
#'
#' @noRd
server_update_solution_settings <- quote({

  # update solution settings
  shiny::observeEvent(input$newSolutionPane_settings, {
    
    ## specify dependencies
    shiny::req(input$newSolutionPane_settings)

    ## update solution settings object
    app_data$ss$set_setting(input$newSolutionPane_settings)
    
    ## pop-up for overlap: exclude
    if ((identical(input$newSolutionPane_settings$type, "exclude")) && input$newSolutionPane_settings$value) {
      name <- app_data$ss$get_exclude_settings()[app_data$ss$get_exclude_settings()$id == input$newSolutionPane_settings$id,]$name
      overlap <- app_data$ss$get_exclude_settings()[app_data$ss$get_exclude_settings()$id == input$newSolutionPane_settings$id,]$overlap
      if (shiny::isTruthy(overlap)) {
        shinyalert::shinyalert(
          title = "Warning",
          text = paste0(name, " overlaps the following includes: ", overlap, ". 
           *Includes take precedence over excludes in prioritization."),
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          type = "warning",
          showConfirmButton = TRUE,
          confirmButtonText = "OK",
          timer = 0,
          confirmButtonCol = "#0275d8",
          animation = TRUE
        )
      }
    }
    
    ## pop-up for overlap: include
    if ((identical(input$newSolutionPane_settings$type, "include")) && input$newSolutionPane_settings$value) {
      name <- app_data$ss$get_include_settings()[app_data$ss$get_include_settings()$id == input$newSolutionPane_settings$id,]$name
      overlap <- app_data$ss$get_include_settings()[app_data$ss$get_include_settings()$id == input$newSolutionPane_settings$id,]$overlap
      if (shiny::isTruthy(overlap)) {
        shinyalert::shinyalert(
          title = "Warning",
          text = paste0(name, " overlaps the following excludes: ", overlap, ". 
           *Includes take precedence over excludes in prioritization."),
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          type = "warning",
          showConfirmButton = TRUE,
          confirmButtonText = "OK",
          timer = 0,
          confirmButtonCol = "#0275d8",
          animation = TRUE
        )
      }
    }    
    
    ## if updating include status,
    ## then update the current amount for each feature within each theme
    if (identical(input$newSolutionPane_settings$type, "include")) {
      ### update object
      app_data$ss$update_current_held(
        theme_data = app_data$theme_data,
        include_data = app_data$include_data
      )

      ### update widget
      vapply(app_data$themes, FUN.VALUE = logical(1), function(x) {
        ### update the widget
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = x$id,
            setting = "feature_current",
            value = x$get_feature_current(),
            type = "theme"
          )
        )
        #### return success
        TRUE
      })
    }
  })
})
