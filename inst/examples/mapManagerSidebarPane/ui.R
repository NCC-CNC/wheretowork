fillPage(
  tagList(
    sidebar_tabs(
      id = "sidebar",
      iconList = list(icon("bars")),
      mapManagerSidebarPane(
        id = "mapManagerPane"
      )
    ),
    leaflet::leafletOutput("map", width = "100%", height = "100%")
  )
)
