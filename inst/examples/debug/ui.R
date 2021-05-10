fluidPage(
  h4("Leaflet Sidebar Plugin"),
  tagList(
    sidebar_tabs(
      id = "mysidebarid",
      iconList = list(icon("car")),
      sidebar_pane(
        title = "New solution",
        id = "paneid",
        icon = NULL,
        solutionSettingsOutput("widget", height = "80vh")
      )
    ),
    leafletOutput("map", height = "700px")
  )
)
