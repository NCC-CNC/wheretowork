fillPage(
  tagList(
    sidebar_tabs(
      id = "sidebar",
      iconList = list(icon("chart-line")),
      solutionResultsSidebarPane(
        id = "solutionResultsPane",
        solutionResultsId = "widget"
      )
    ),
    leafletOutput("map", width = "100%", height = "100%")
  )
)
