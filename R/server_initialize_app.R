#' Sever function: initialize application
#'
#' Set behavior for initializing the application.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_initialize_app)
#' ```
#'
#' @noRd
server_initialize_app <- quote({

  # define application data
  ## note that we use an environment here because they are mutable objects and
  ## so we don't have to worry about using the super-assignment operator
  app_data <- list2env(
    list(
      ## file paths
      configuration_path = NULL,
      spatial_path = NULL,
      boundary_path = NULL,
      attribute_path = NULL,
      ## settings
      mode = NULL,
      mapping = NULL,
      ## objects
      dataset = NULL,
      themes = NULL,
      weights = NULL,
      includes = NULL,
      solutions = list(),
      cache = cachem::cache_mem(),
      ## data
      bbox = NULL,
      theme_data = NULL,
      weight_data = NULL,
      include_data = NULL,
      area_data = NULL,
      boundary_data = NULL,
      solution_ids = character(0),
      ## widgets
      mm = NULL,
      ss = NULL,
      ## state variables
      new_solution_id = NULL,
      task = NULL
    )
  )

  # activate start up mode
  ## hides leaflet buttons + scalebar
  shinyjs::runjs("document.body.classList.add('startup');")

  # make sidebars hidden
  shinyjs::runjs("$('#dataSidebar').css('display','none');")
  shinyjs::runjs("$('#analysisSidebar').css('display','none');")

  # display import modal on start up
  shiny::showModal(importModal(id = "importModal"))

  # initialize map
  output$map <- leaflet::renderLeaflet({
    leaflet_map(c("dataSidebar", "analysisSidebar"))
  })

  # initialize spatial import settings
  output$importModal_spatial_settings <- renderImportSettings({
    importSettings(buttonId = "importModal_spatial_button")
  })

  # initialize widgets
  output$solutionResultsPane_results <- renderSolutionResults({
    solutionResults()
  })

  # initialize built in projects
  if (nrow(project_data) > 0) {
    ## update select input with project names
    shiny::updateSelectInput(
      inputId = "importModal_name",
      choices = stats::setNames(project_data$path, project_data$name)
    )
  } else {
    ## disable import button since no available projects
    disable_html_element("importModal_builtin_button")
  }

  # disable buttons that require inputs
  disable_html_element("importModal_manual_button")
  disable_html_element("importModal_spatial_button")
  shinyjs::disable("exportPane_button")
  shinyjs::disable("newSolutionPane_settings_stop_button")

  # disable solution results sidebar button
  disable_html_css_selector("#analysisSidebar li:nth-child(2)")

  # manually update solution settings sidebar content,
  # if can't manually stop processing
  if (!identical(strategy, "multicore")) {
    # hide solution stop button if not supported
    shinyjs::runjs("$('.solution-footer-stop-button').hide()")
    # resize the start button
    shinyjs::runjs(
      "$('#newSolutionPane_settings_start_button').css('width','150px;')"
    )
  }

  # hide elements
  shinyjs::hideElement("importModal_spatial_text")

  # add help modal button trigger
  observeEvent(input$help_button, {
    shinyBS::toggleModal(
      session = session, modalId = "helpModal", toggle = "open"
    )
  })

})
