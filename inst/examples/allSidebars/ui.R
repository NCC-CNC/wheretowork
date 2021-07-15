fillPage(
  tagList(
    # enable pretty scrollbars
    useCustomScrollbars(),

    # map
    leafletOutput("map", width = "100%", height = "100%"),

    # data sidebar (appears on left)
    sidebar_tabs(
      id = "dataSidebar",
      iconList = list(
        shiny::icon("bars"),
        shiny::icon("download"),
        shiny::icon("heart"),
        shiny::icon("envelope")
      ),
      mapManagerSidebarPane(id = "mapManagerPane"),
      exportSidebarPane(id = "exportPane"),
      acknowledgmentsSidebarPane(id = "acknowledgmentsPane"),
      contactSidebarPane(id = "contactPane")
    ),

    # analysis sidebar (appears on right)
    sidebar_tabs(
      id = "analysisSidebar",
      iconList = list(shiny::icon("rocket"), icon("chart-line")),
      newSolutionSidebarPane(id = "newSolutionPane"),
      solutionResultsSidebarPane(id = "solutionResultsPane")
    )
  )
)
