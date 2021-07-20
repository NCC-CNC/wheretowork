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

  # create reactive value to store new solutions
  user_solution <- shiny::reactiveVal()

  # generate new solution when button pressed
  shiny::observeEvent(
    input$newSolutionPane_settings_button,
    {
      ## specify dependencies
      shiny::req(input$newSolutionPane_settings_button)
      shiny::req(input$newSolutionPane_settings_name)
      shiny::req(input$newSolutionPane_settings_color)

      ## update generate solution inputs
      disable_html_element("newSolutionPane_settings_button")
      disable_html_element("newSolutionPane_settings_name")
      disable_html_element("newSolutionPane_settings_color")

      ### preliminary calculations
      curr_name <- input$newSolutionPane_settings_name
      curr_gap <- get_golem_config("gap")
      curr_boundary_value <-
        (app_data$ss$get_parameter("spatial_parameter")$value *
          app_data$ss$get_parameter("spatial_parameter")$status) / 100
      curr_color <- scales::alpha(input$newSolutionPane_settings_color, 0.8)
      curr_type <- app_data$ss$get_parameter("budget_parameter")$status
      curr_area_budget <-
        app_data$ss$get_parameter("budget_parameter")$value / 100

      ## generate solution
      future::future(packages = "wheretowork", seed = NULL, {
        if (curr_type) {
          ### if budget specified, then use the min shortfall formulation
          s <- try(
            min_shortfall_solution(
              name = curr_name,
              area_budget_proportion = curr_area_budget,
              dataset = app_data$dataset,
              settings = app_data$ss,
              theme_data = app_data$theme_data,
              weight_data = app_data$weight_data,
              include_data = app_data$include_data,
              boundary_data = app_data$boundary_data,
              gap = curr_gap,
              boundary_budget_proportion = curr_boundary_value,
              legend_color = curr_color,
              cache = app_data$cache
            ),
            silent = TRUE
          )
        } else {
          ### else, then use the min set formulation
          s <- try(
            min_set_solution(
              name = curr_name,
              dataset = app_data$dataset,
              settings = app_data$ss,
              theme_data = app_data$theme_data,
              weight_data = app_data$weight_data,
              include_data = app_data$include_data,
              boundary_data = app_data$boundary_data,
              gap = curr_gap,
              boundary_gap = curr_boundary_value,
              legend_color = curr_color,
              cache = app_data$cache
            ),
            silent = TRUE
          )
        }
        list(solution = s, cache = app_data$cache)
      }) %...>%
      (function(result) {
        user_solution(result$solution)
        app_data$cache <- result$cache
      }) %...!%
        (function(error) {
          user_solution(NULL)
          warning(error)
      })
      ## this needed to implement asynchronous processing,
      ## see https://github.com/rstudio/promises/issues/23
      NULL
    }
  )

  # add solution to map when generating new solution
  shiny::observeEvent(user_solution(), {
    ## specify dependencies
    if (is.null(user_solution())) {
      return()
    }

    ## extract solution
    s <- user_solution()

    ## if failed to generate solution...
    if (inherits(s, "try-error")) {
      ## identify error message to show
      msg <- switch(attr(s, "condition")$message,
        "code_1" = paste(
          "The \"Total area budget\" setting is too low given the selected",
          "Includes. Try increasing the total area budget or deselecting ",
          " some of the Includes."
        ),
        "Something went wrong, please try again."
      )
      ## display modal
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
      ## reset button
      shinyFeedback::resetLoadingButton("newSolutionPane_settings_button")
      enable_html_element("solutionResultsPane_results_button")
      enable_html_element("newSolutionPane_settings_color")
      enable_html_element("newSolutionPane_settings_name")
      ## exit
      return()
    }

    ## since the asynchronous processing creates a solution and
    ## stores it's values in a new dataset object, we need to:
    ## (1) copy the solution values into app_data$dataset and
    ## (2) update the solution to look in app_data$dataset for its values
    app_data$dataset$add_index(
      index = s$variable$index,
      values = s$variable$dataset$attribute_data[[s$variable$index]]
    )
    s$variable$dataset <- app_data$dataset

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
    enable_html_element("newSolutionPane_settings_name")

    ## enable solution results modal button after generating first solution
    if (length(app_data$solutions) == 1) {
      enable_html_element("solutionResultsPane_results_button")
      enable_html_css_selector("#analysisSidebar li:nth-child(2)")
    }

    ## reset generate new solution buttons
    shinyFeedback::resetLoadingButton("newSolutionPane_settings_button")
    disable_html_element("newSolutionPane_settings_button")
    enable_html_element("newSolutionPane_settings_color")
  })
})
