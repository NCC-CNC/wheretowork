#' Sever function: update map
#'
#' Set behavior for updating the Leaflet map.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_update_map)
#' ```
#'
#' @noRd
server_update_map <- quote({

  # update map based on map manager widget
  shiny::observeEvent(input$mapManagerPane_settings, {
    ## specify dependencies
    shiny::req(input$mapManagerPane_settings)

    ## generate leaflet proxy
    map <- leaflet::leafletProxy("map")

    ## update map manager
    if (!identical(input$mapManagerPane_settings$setting, "remove")) {
      ### if updating order or visibility, then update the map manager object
      app_data$mm$set_setting(input$mapManagerPane_settings)
    } else {
      ### if removing a layer...
      ### check if layer is a solution
      i <- which(app_data$solution_ids == input$mapManagerPane_settings$id)

      ### remove solution if needed
      if (length(i) > 0) {
        ### remove from app data
        app_data$solutions <- app_data$solutions[-i]
        app_data$solution_ids <- app_data$solution_ids[-i]

        ### remove from solution results widget
        dropSolutionResults(
          session = session,
          inputId = "solutionResultsPane_results",
          value = input$mapManagerPane_settings$id
        )
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "solutionResultsPane_results_select",
          choices = app_data$solution_ids
        )

        ### if no solutions remain...
        if (length(app_data$solutions) == 0) {
          #### disable solution results sidebar button
          disable_html_css_selector("#analysisSidebar li:nth-child(2)")
          #### open solution settings sidebar
          leaflet.extras2::openSidebar(
            map,
            id = "newSolutionPane", sidebar_id = "analysisSidebar"
          )
        }
      }

      ### remove from map manager widget
      dropMapManagerLayer(
        session = session,
        inputId = "mapManagerPane_settings",
        value = input$mapManagerPane_settings$id
      )

      ### remove from map
      app_data$mm$drop_layer(
        value = input$mapManagerPane_settings$id,
        map = map
      )
    }
    
    ## update map
    app_data$mm$update_map(map)
    ## delete the oldest loaded and invisible map pane (if cache is > 3)
    app_data$mm$delete_sinlge_map_pane(map)    
  })

  # update map based on hide button
  shiny::observeEvent(input$hide_button, {
    ## specify dependencies
    shiny::req(input$hide_button)

    ## update map manager object
    app_data$mm$set_visible(FALSE)

    ## update map manager widget
    vapply(app_data$mm$layers, FUN.VALUE = logical(1), function(x) {
      updateMapManagerLayer(
        session = session,
        inputId = "mapManagerPane_settings",
        value = list(
          id = x$id,
          setting = "visible",
          value = FALSE
        )
      )
      TRUE
    })

    ## delete all map panes
    app_data$mm$delete_all_map_panes(leaflet::leafletProxy("map"))
  })

  # update map based on home button
  shiny::observeEvent(input$home_button, {
    ## specify dependencies
    shiny::req(input$home_button)

    ## update map
    leaflet::flyToBounds(
      map = leaflet::leafletProxy("map"),
      lng1 = app_data$bbox$xmin,
      lat1 = app_data$bbox$ymin,
      lng2 = app_data$bbox$xmax,
      lat2 = app_data$bbox$ymax
    )
  })

  # update map based on print button
  shiny::observeEvent(input$print_button, {
    ## specify dependencies
    shiny::req(input$print_button)

    ## save screenshot of map
    leaflet.extras2::easyprintMap(
      map = leaflet::leafletProxy("map"),
      sizeModes = "A4Landscape",
      filename = "map"
    )
  })
})
