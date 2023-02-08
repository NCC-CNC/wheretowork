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
     # set user settings
     app_data$ss$set_user_settings()
     ### get user settings... 
     ### return class "try-error" if user input config does not match project
     x <- try(app_data$ss$get_user_settings(),
              silent = TRUE
            )
     
     if (identical(class(x), "try-error")) {
       msg <- paste(
        "Input configurations do not match current project.", 
        "Be sure to upload a *_configs.yaml file previously downloaded from", 
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
     }
    
    ## update settings with user uploaded config file
    if ((length(app_data$ss$user_settings) > 0) & is.list(x)) {
      
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
        ### update weight factor
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
      ### update includes status
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
      ### update excludes status
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
      ### update parameter status
      lapply(seq_along(app_data$ss$parameters), function(x) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$parameters[[x]]$id,
            setting = "status",
            value = app_data$ss$parameters[[x]]$status,
            type = "parameter"
          )
        )
      })
      ### update parameter value
      lapply(seq_along(app_data$ss$parameters), function(x) {
        updateSolutionSettings(
          inputId = "newSolutionPane_settings",
          value = list(
            id = app_data$ss$parameters[[x]]$id,
            setting = "value",
            value = app_data$ss$parameters[[x]]$value,
            type = "parameter"
          )
        )
      })       
      
    }
   }
    
    ## if updating include status,
    ## then update the current amount for each feature within each theme
    if ((identical(input$newSolutionPane_settings$type, "include")) |
      (length(app_data$ss$user_settings) > 0))
      {
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
