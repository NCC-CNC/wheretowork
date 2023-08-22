#' Sever function: generate new solution
#'
#' Set behavior for generating new solutions.
#'
#' @param input,output,session Arguments inherited from [shiny::shinyServer].
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
server_generate_new_solution <- quote({

  # create reactive value to store new results
  new_user_result <- shiny::reactiveVal()

  # stop processing when stop button solution pressed
  shiny::observeEvent(input$newSolutionPane_settings_stop_button, {
    ## specify dependencies
    shiny::req(input$newSolutionPane_settings_stop_button)
    shiny::req(app_data$new_solution_id)

    ## stop processing if possible given strategy
    if (identical(strategy, "multicore")) {
      ## kill task if possible
      suppressWarnings(ipc::stopMulticoreFuture(app_data$task))

      ## reset app state
      app_data$new_solution_id <- NULL

      ## reset buttons and input widgets
      shinyFeedback::resetLoadingButton("newSolutionPane_settings_start_button")
      enable_html_element("newSolutionPane_settings_start_button")
      enable_html_element("newSolutionPane_settings_name")
      enable_html_element("newSolutionPane_settings_color")
      shinyjs::disable("newSolutionPane_settings_stop_button")
    }
  })

  # generate new solution when start button pressed
  shiny::observeEvent(input$newSolutionPane_settings_start_button, {
    
    ## specify dependencies
    shiny::req(input$newSolutionPane_settings_start_button)
    shiny::req(input$newSolutionPane_settings_name)
    shiny::req(input$newSolutionPane_settings_color)

    ## update generate solution inputs
    disable_html_element("newSolutionPane_settings_start_button")
    disable_html_element("newSolutionPane_settings_name")
    disable_html_element("newSolutionPane_settings_color")
    disable_html_element("newSolutionPane_settings_gurobi")

    ## generate id and store it in app_data
    curr_id <- uuid::UUIDgenerate()
    app_data$new_solution_id <- curr_id

    ## extract values for generating result
    ### settings
    curr_theme_settings <- app_data$ss$get_theme_settings()
    curr_weight_settings <- app_data$ss$get_weight_settings()
    curr_include_settings <- app_data$ss$get_include_settings()
    curr_exclude_settings <- app_data$ss$get_exclude_settings()
    ### data
    curr_area_data <- app_data$area_data
    curr_boundary_data <- app_data$boundary_data
    curr_theme_data <- app_data$theme_data
    curr_weight_data <- app_data$weight_data
    curr_include_data <- app_data$include_data
    curr_exclude_data <- app_data$exclude_data
    ### arguments for generating result
    curr_time_limit_1 <- get_golem_config("solver_time_limit_1")
    curr_time_limit_2 <- get_golem_config("solver_time_limit_2")
    curr_name <- input$newSolutionPane_settings_name
    curr_gap_1 <- get_golem_config("solver_gap_1")
    curr_gap_2 <- get_golem_config("solver_gap_2")
    curr_verbose <- get_golem_config("verbose")
    curr_color <- scales::alpha(input$newSolutionPane_settings_color, 0.8)
    curr_type <- app_data$ss$get_parameter("budget_parameter")$status
    curr_cache <- app_data$cache
    curr_area_budget <- c(
      app_data$ss$get_parameter("budget_parameter")$value *
      app_data$ss$get_parameter("budget_parameter")$status
    ) / 100
    curr_boundary_gap <- c(
      app_data$ss$get_parameter("spatial_parameter")$value *
      app_data$ss$get_parameter("spatial_parameter")$status
    ) / 100
    curr_parameters <- lapply(app_data$ss$parameters, function(x) x$clone())
    curr_overlap <- app_data$ss$get_parameter("overlap_parameter")$status
    #### gurobi web license server check-in
    try_gurobi <- input$newSolutionPane_settings_gurobi

    ## if failed to generate solution...
    if (!any(curr_theme_settings$status > 0.5)) {
      ### identify error message to show
      msg <- paste(
        "All Themes have been disabled.",
        "In order to generate a solution,",
        "please enable at least one Theme and try again."
      )
      ### display modal
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
      ### reset buttons
      shinyFeedback::resetLoadingButton("newSolutionPane_settings_start_button")
      disable_html_element("newSolutionPane_settings_name")
      disable_html_element("newSolutionPane_settings_color")
      ## exit
      return()
    }


      ## enable stop button
      shinyjs::enable("newSolutionPane_settings_stop_button")

      ## generate result using asynchronous task
      app_data$task <- future::future(packages = "wheretowork", seed = NULL, {
        ### main processing
        if (curr_type) {
          #### if budget specified, then use the min shortfall formulation
          r <- try(
            min_shortfall_result(
              id = curr_id,
              area_budget_proportion = curr_area_budget,
              area_data = curr_area_data,
              boundary_data = curr_boundary_data,
              theme_data = curr_theme_data,
              weight_data = curr_weight_data,
              include_data = curr_include_data,
              exclude_data = curr_exclude_data,
              theme_settings = curr_theme_settings,
              weight_settings = curr_weight_settings,
              include_settings = curr_include_settings,
              exclude_settings = curr_exclude_settings,
              parameters = curr_parameters,
              overlap = curr_overlap,
              gap_1 = curr_gap_1,
              gap_2 = curr_gap_2,
              boundary_gap = curr_boundary_gap,
              cache = curr_cache,
              time_limit_1 = curr_time_limit_1,
              time_limit_2 = curr_time_limit_2,
              verbose = curr_verbose
            ),
            silent = TRUE
          )
        } else {
          #### else, then use the min set formulation
          r <- try(
            min_set_result(
              id = curr_id,
              area_data = curr_area_data,
              boundary_data = curr_boundary_data,
              theme_data = curr_theme_data,
              weight_data = curr_weight_data,
              include_data = curr_include_data,
              exclude_data = curr_exclude_data,
              theme_settings = curr_theme_settings,
              weight_settings = curr_weight_settings,
              include_settings = curr_include_settings,
              exclude_settings = curr_exclude_settings,
              parameters = curr_parameters,
              overlap = curr_overlap,
              gap_1 = curr_gap_1,
              gap_2 = curr_gap_2,
              boundary_gap = curr_boundary_gap,
              cache = curr_cache,
              time_limit_1 = curr_time_limit_1,
              time_limit_2 = curr_time_limit_2,
              verbose = curr_verbose,
              try_gurobi = try_gurobi
            ),
            silent = TRUE
          )
        }
        ## return result
        list(
          id = curr_id, name = curr_name, color = curr_color,
          result = r, cache = curr_cache
        )
      })
      ## add promises to handle result once asynchronous task finished
      prom <-
        (app_data$task) %...>%
        (function(result) {
          new_user_result(result)
          app_data$cache <- result$cache
        }) %...!%
        (function(error) {
          new_user_result(NULL)
          if (!is.null(app_data$new_solution_id)) {
            warning(error)
          }
          NULL
        })

      ## this needed to implement asynchronous processing,
      ## see https://github.com/rstudio/promises/issues/23
      NULL
    }
  )

  # add solution to map when generating new solution
  shiny::observeEvent(new_user_result(), {
    ## specify dependencies
    if (is.null(new_user_result()) || is.null(app_data$new_solution_id)) {
      return()
    }
    if (!identical(new_user_result()$id, app_data$new_solution_id)) {
      return()
    }

    ## disable stop button
    shinyjs::disable("newSolutionPane_settings_stop_button")

    ## extract result
    r <- new_user_result()

    ## if failed to generate solution...
    if (inherits(r$result, "try-error")) {
      ### identify error message to show
      msg <- attr(r$result, "condition")$message
      print(msg)
      if(startsWith(msg, "WtW:")) {
        msg <- gsub("WtW: ", "", msg, fixed = TRUE)
      } else if (startsWith(msg, "no solution found")) {
        msg <- paste0(
          "No solution found due to problem infeasibility. ", 
          "This is likely caused by a weight setting confilcting with the total",
          " area budget. Try setting your weight(s) closer to 0."
        ) 
      } else if (startsWith(msg, "Error 10009:")) {
        msg <- paste0(
          "Another Gurobi process is running. ",
          "Only one license can be used at a time. ",
          "Try again, or untoggle the Gurobi swith to use the defaut open source solver."
          )
      } else {
        msg <- "Something went wrong, please try again."
      }

      ### throw warning in development mode
      if (golem::app_dev()) {
        whereami::whereami()
        cli::cli_verbatim(r$result)
        cli::rule()
      }
      ### display modal
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
      ### reset button
      shinyFeedback::resetLoadingButton("newSolutionPane_settings_start_button")
      enable_html_element("newSolutionPane_settings_color")
      enable_html_element("newSolutionPane_settings_name")
      ## exit
      return()
    }

    ## generate solution from result
    s <- new_solution_from_result(
      id = uuid::UUIDgenerate(),
      result = r$result,
      name = r$name,
      visible = if (app_data$ss$get_parameter("solution_layer_parameter")$status) FALSE else TRUE,
      hidden = app_data$ss$get_parameter("solution_layer_parameter")$status,
      dataset = app_data$dataset,
      settings = app_data$ss,
      legend = new_manual_legend(
        values = c(0, 1),
        colors = c("#00FFFF00", r$color),
        labels = c("not selected", "selected")
      )
    )
    rm(r)

    ## make leaflet proxy
    map <- leaflet::leafletProxy("map")

    ## store solution
    app_data$solutions <- append(app_data$solutions, list(s))

    ## store solution id and names
    app_data$solution_ids <-
      c(app_data$solution_ids, stats::setNames(s$id, s$name))

    ## add new solution to the map
    app_data$mm$add_layer(s, map)

    ## add new solution to map manager widget
    addMapManagerLayer(
      session = session,
      inputId = "mapManagerPane_settings",
      value = s
    )

    ## add new solution to solution results widget
    addSolutionResults(
      session = session,
      inputId = "solutionResultsPane_results",
      value = s
    )

    ## add new solution to export sidebar
    shiny::updateSelectizeInput(
      session = session,
      inputId = "exportPane_fields",
      choices = stats::setNames(
        app_data$mm$get_layer_indices(),
        app_data$mm$get_layer_names()
      )
    )

    ## add new solution to solution results modal
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "solutionResultsPane_results_modal_select",
      choices = app_data$solution_ids,
      selected = dplyr::last(app_data$solution_ids)
    )

    ## show the new solution in the results widget
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "solutionResultsPane_results_select",
      choices = app_data$solution_ids,
      selected = dplyr::last(app_data$solution_ids)
    )
    showSolutionResults(
      session = session,
      inputId = "solutionResultsPane_results",
      value = s$id
    )

    ## show solution results sidebar
    leaflet.extras2::openSidebar(
      map,
      id = "solutionResultsPane", sidebar_id = "analysisSidebar"
    )

    ## reset solution name
    shiny::updateTextInput(
      session = session,
      inputId = "newSolutionPane_settings_name",
      value = ""
    )

    ## enable solution results modal button after generating first solution
    if (length(app_data$solutions) == 1) {
      enable_html_css_selector("#analysisSidebar li:nth-child(2)")
    }

    ## reset buttons and input widgets
    shinyFeedback::resetLoadingButton("newSolutionPane_settings_start_button")
    enable_html_element("newSolutionPane_settings_name")
    enable_html_element("newSolutionPane_settings_color")
    enable_html_element("newSolutionPane_settings_gurobi")
    disable_html_element("newSolutionPane_settings_start_button")
  })

})
