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
    
    ## set user configs on file upload
   if(input$newSolutionPane_settings$setting == "fileinput") {
     app_data$ss$set_user_settings()
     app_data$ss$update_settings()
    }
    
    ## update settings with user uploaded config file
    if (length(app_data$ss$user_settings) > 0) {
      ### update the widget - weights
      lapply(seq_along(app_data$weights), function(x) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$weights[[x]]$id,
            setting = "status",
            value = app_data$ss$weights[[x]]$status,
            type = "weight"
          )
        )
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$weights[[x]]$id,
            setting = "factor",
            value = app_data$ss$weights[[x]]$factor,
            type = "weight"
          )
        )        
      })      
      ### update the widget - includes
      lapply(seq_along(app_data$includes), function(x) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$includes[[x]]$id,
            setting = "status",
            value = app_data$ss$includes[[x]]$status,
            type = "include"
          )
         )
       })
      ### update the widget - excludes
      lapply(seq_along(app_data$excludes), function(x) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$excludes[[x]]$id,
            setting = "status",
            value = app_data$ss$excludes[[x]]$status,
            type = "exclude"
          )
        )
      })      
      
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
