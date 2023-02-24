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
    
   ## listen for file input parameter setting
   if(input$newSolutionPane_settings$setting == "fileinput") {
     ### read-in user uploaded configuration settings as list
     settings_lst <- yaml::yaml.load(input$newSolutionPane_settings$value)
     ### update solution settings 
     updated_ss <- try(app_data$ss$update_ss(settings_lst), silent = TRUE)
     ### update solution settings widget
     if (identical(class(updated_ss), "try-error")) {
       # Update file icon 
       change_file_icon_js(".file-container i")
       
       msg <- paste(
        "Update solution settings .yaml file does not match current project.", 
        "Be sure to upload a .yaml file previously downloaded from", 
        "this project for toggle switches and slider values to match a previous",
        "optimization run."
       )
       ### display error modal
       shinyalert::shinyalert(
         title = "Oops",
         text = msg,
         size = "s",
         closeOnEsc = TRUE,
         closeOnClickOutside = TRUE,
         type = "error",
         showConfirmButton = TRUE,
         confirmButtonText = "OK",
         timer = 0,
         confirmButtonCol = "#0275d8",
         animation = TRUE
       )
     } else {
      ### update theme/feature status
      vapply(app_data$themes, FUN.VALUE = logical(1), function(x) {
        if ((!all(x$get_feature_status())) &
            (length(x$get_feature_status()) > 1)) {
          ### update group status
          updateSolutionSettings(
            inputId = "newSolutionPane_settings",
            value = list(
              id = x$id,
              setting = "status",
              value = FALSE,
              type = "theme"
            )
          )
        } else {
          ### update feature status
          updateSolutionSettings(
            inputId = "newSolutionPane_settings",
              value = list(
              id = x$id,
              setting = "feature_status",
              value = x$get_feature_status(),
              type = "theme"
            )
          )
        }
       #### return success
       TRUE
      })
      ### update theme/feature goal
      vapply(app_data$themes, FUN.VALUE = logical(1), function(x) {
        if ((length(unique(x$get_feature_goal())) == 1) &
            (length(x$get_feature_goal()) > 1)) {
          #### update group goal
          updateSolutionSettings(
            inputId = "newSolutionPane_settings",
            value = list(
              id = x$id,
              setting = "group_goal",
              value = unique(x$get_feature_goal()),
              type = "theme"
            )
          )
          ### update view to group tab
          updateSolutionSettings(
            inputId = "newSolutionPane_settings",
            value = list(
              id = x$id,
              setting = "view",
              value = "group",
              type = "theme"
            )
          )          
        } else {
          ### update feature goal
          updateSolutionSettings(
            inputId = "newSolutionPane_settings",
            value = list(
              id = x$id,
              setting = "feature_goal",
              value = x$get_feature_goal(),
              type = "theme"
            )
          )
          ### update view to single tab
          updateSolutionSettings(
            inputId = "newSolutionPane_settings",
            value = list(
              id = x$id,
              setting = "view",
              value = "single",
              type = "theme"
            )
          )          
        }    
        #### return success
        TRUE
      })       
      ### update weights status
      lapply(seq_along(app_data$weights), function(i) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$weights[[i]]$id,
            setting = "status",
            value = app_data$ss$weights[[i]]$status,
            type = "weight"
          )
        )
        ### update weight factor
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$weights[[i]]$id,
            setting = "factor",
            value = app_data$ss$weights[[i]]$factor,
            type = "weight"
          )
        )
      })
      ### update includes status
      lapply(seq_along(app_data$includes), function(i) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$includes[[i]]$id,
            setting = "status",
            value = app_data$ss$includes[[i]]$status,
            type = "include"
          )
         )
       })
      ### update excludes status
      lapply(seq_along(app_data$excludes), function(i) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$excludes[[i]]$id,
            setting = "status",
            value = app_data$ss$excludes[[i]]$status,
            type = "exclude"
          )
        )
      })
      ### update parameter status
      lapply(seq_along(app_data$ss$parameters), function(i) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$parameters[[i]]$id,
            setting = "status",
            value = app_data$ss$parameters[[i]]$status,
            type = "parameter"
          )
        )
      })
      ### update parameter value
      lapply(seq_along(app_data$ss$parameters), function(i) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$parameters[[i]]$id,
            setting = "value",
            value = app_data$ss$parameters[[i]]$value,
            type = "parameter"
          )
        )
      })       
     }
    }
    
    ## if updating include status,
    ## then update the current amount for each feature within each theme
    if ((identical(input$newSolutionPane_settings$type, "include")) |
       (exists("updated_ss") && identical(class(updated_ss), "list"))) {
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
