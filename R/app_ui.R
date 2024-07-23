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

      ## suppress dependencies that fail to import correctly
      htmltools::suppressDependencies("shinyBS"),
      htmltools::suppressDependencies("bootstrap-select"),

      ## manually insert code dependencies so they import correctly
      htmltools::tags$head(
        ### unblock mixed content
        htmltools::tags$meta(
          "http-equiv"="Content-Security-Policy", "content"="upgrade-insecure-requests"),
        ### shinyBS just doesn't work inside Docker containers
        htmltools::tags$script(src = "www/shinyBS-copy.js"),
        ### shinyWidgets has invalid SourceMap configuration
        htmltools::tags$script(src = "www/bootstrap-select-copy.min.js"),
        htmltools::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "www/bootstrap-select-copy.min.css"
        )
      ),

      ## start up screen
      shinybusy::busy_start_up(
        loader = shinybusy::spin_epic("scaling-squares", color = "#FFF"),
        text = "Loading...",
        mode = "auto",
        color = "#FFF",
        background = "#001329"
      ),

      ## title
      shiny::h3(
        "Where To Work",
        id = "app_title",
        class = "leaflet-title"
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
#' @details
#' This function can also add Google Analytics tracking to the web application.
#' To achieve this, you need to specify the Google Analytics Identifier using
#' the `GOOGLE_ANALYTICS_ID` environmental variable.
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

    ## dependencies
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
  )
}
