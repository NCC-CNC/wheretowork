fillPage(
  htmltools::htmlDependencies(icon("home")),
  tagList(
    leaflet::leafletOutput("map", width = "100%", height = "100%"),
    sidebar_tabs(
      id = "sidebar",
      iconList = list(icon("bars"), icon("download")),
      mapManagerSidebarPane(id = "mapManagerPane"),
      exportSidebarPane(id = "exportPane")
    )
  )
)
