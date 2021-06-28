fillPage(
  tagList(
    sidebar_tabs(
      id = "sidebar",
      iconList = list(icon("bars"), icon("download")),
      mapManagerSidebarPane(id = "mapManagerPane"),
      exportSidebarPane(id = "exportPane")
    ),
    leaflet::leafletOutput("map", width = "100%", height = "100%")
  )
)
