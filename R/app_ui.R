#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  htmltools::tagList(
    # add external resources
    golem_add_external_resources(),

    # app content
    shiny::fillPage(

      ## manually insert shinyBS JS code
      htmltools::tags$script(src = "www/shinyBS-copy.js"),

      ## start up screen
      shinybusy::busy_start_up(
        loader = shinybusy::spin_epic("scaling-squares", color = "#FFF"),
        text = "Loading...",
        mode = "auto",
        color = "#FFF",
        background = "#001329"
      ),

      ## leaflet map
      leaflet::leafletOutput("map", width = "100%", height = "100%"),

      ## help modal
      helpModal("helpModal", trigger = "help_button"),

      ## data sidebar (appears on left)
      leaflet.extras2::sidebar_tabs(
        id = "dataSidebar",
        iconList = list(
          shiny::icon("layer-group"),
          shiny::icon("download"),
          shiny::icon("envelope"),
          shiny::icon("heart")
        ),
        mapManagerSidebarPane(id = "mapManagerPane"),
        exportSidebarPane(id = "exportPane"),
        contactSidebarPane(id = "contactPane"),
        acknowledgmentsSidebarPane(id = "acknowledgmentsPane")
      ),

      ## analysis sidebar (appears on right)
      leaflet.extras2::sidebar_tabs(
        id = "analysisSidebar",
        iconList = list(
          shiny::icon("rocket"),
          shiny::icon("tachometer-alt")
        ),
        newSolutionSidebarPane(id = "newSolutionPane"),
        solutionResultsSidebarPane(id = "solutionResultsPane")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # add resources
  golem::add_resource_path(
    "www", app_sys("app/www")
  )

  # define HTML tags in header
  htmltools::tags$head(
    ## bundle CSS and JS files
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Where To Work"
    ),

    ## favicon
    golem::favicon(),

    ## dependencies
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert()
  )
}
